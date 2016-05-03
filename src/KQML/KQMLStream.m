/* -*- mode: objc; -*-
 *
 * File: KQMLStream.m
 * Creator: George Ferguson
 * Created: Fri Jan  8 09:09:32 2010
 * Time-stamp: <Tue Jan 12 15:24:51 EST 2010 ferguson>
 *
 * Cocoa interface to the KQML library. This class sets up and handles
 * Cocoas streams, passing the data from the input side to the C version
 * of the KQML library routines. Output works similarly in reverse.
 *
 * Note that C KQML library is not at all object-oriented. So I didn't
 * spend much effort trying to make the Objective-C version any better
 * (more like the Java version, in fact). For example, we just pass
 * around C strings coming from the C library rather than turning them
 * into objects like lists and tokens, etc., or even into NSStrings.
 * If we start doing serious development in ObjC, this should be
 * redone and not reused.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <Foundation/NSHost.h>
#include <Foundation/NSRunLoop.h>
#include <Foundation/NSString.h>
#include <Foundation/NSError.h>
#include "KQMLStream.h"

@implementation KQMLStream

/*
 * Allocate and return a new KQMLStream connected to given host/port.
 * Returns the new object if successful, else nil.
 */
+ (id)connectedToHost:(NSString*)hostname port:(int)port {
    KQMLStream *stream = [KQMLStream alloc];
    if ([stream connectToHost:hostname port:port]) {
		return stream;
    } else {
		return nil;
    }
}

/*
 * Connect to given host port and setup IO streams. Return YES if successful,
 * else NO.
 */
- (BOOL)connectToHost:(NSString*)hostname port:(int)port {
    //NSLog(@"KQMLStream:connectToHost: connecting to %@:%d", hostname, port);
    // Setup KQML context
    context = KQMLNewContext();
    // Setup socket stream(s)
    NSHost *host = [NSHost hostWithName:hostname];
    [NSStream getStreamsToHost:host port:port inputStream:&in outputStream:&out];
    if (in == nil || out == nil) {
		// This seems to never be nil but will fail later (see no-op below)
		NSLog(@"KQMLStream: couldn't connect to host: %@:%d", hostname, port);
		return NO;
    }
	//NSLog(@"KQMLStream: in=%@, out=%@", in, out);
    [in retain];
    [out retain];
    [in setDelegate:self];
    [out setDelegate:self];
    [in scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [out scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [in open];
    [out open];
	// This almost no-op causes error (eg., connection refused) to be reported before we start writing
	[out write:(unsigned char*)"\n" maxLength:1];
    //NSLog(@"KQMLStream:connectToHost: connected!");
    return YES;
}

/*
 * Do what's necessary to cleanup a KQMLStream.
 */
- (void)close {
    [in close];
    [out close];
    [in removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [out removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [in release];
    [out release];
	in = nil;
	out = nil;
}

/*
 * Delegate get/set methods.
 */
- (id)delegate {
    return _delegate;
}

- (void)setDelegate:(id<KQMLStreamDelegate>)delegate {
    _delegate = delegate;
}

/*
 * NSStream delegate method
 * It seems a bit strange to have the same delegate (handler) for both the
 * input and the output side, but maybe we can get away with it.
 */
- (void)stream:(NSStream *)stream handleEvent:(NSStreamEvent)eventCode {
    switch(eventCode) {
		case NSStreamEventHasBytesAvailable: // Assume input side...
		{
			uint8_t buf[1024];
			unsigned int len = 0;
			//NSLog(@"KQMLStream:stream:handleEvent: reading...");
			len = [(NSInputStream *)stream read:buf maxLength:1024];
			//NSLog(@"KQMLStream:stream:handleEvent: read %d bytes", len);
			if (len) {
				int i;
				for (i=0; i < len; i++) {
					KQMLError error;
					KQMLPerformative *perf = KQMLInputChar(buf[i], context, &error);
					if (perf) {
						// Done!
						//NSLog(@"KQMLStream: read performative");
						// Pass performative to delegate...
						[_delegate KQMLStream:self receivedMessage:perf];
						// Free memory (delegate must copy to save)
						KQMLFreePerformative(perf);
					} else if (error < 0) {
						// Error
						NSLog(@"KQMLStream:stream:handleEvent: KQML error %d: %s", error, KQMLErrorString(error));
						// FIXME: Throw an exception or something
					} else {
						// Not finished parsing
					}
				}
			}
			break;
		}
		case NSStreamEventEndEncountered:
		{
			//NSLog(@"KQMLStream:stream:handleEvent: EOF");
			[self close];
			[_delegate eofOnKQMLStream:self];
			break;
		}
        case NSStreamEventErrorOccurred:
        {
            NSError *err = [stream streamError];
			NSLog(@"KQMLStream: error %i on stream %@: %@", [err code], stream, [err localizedDescription]);
			// Technically should be smarter about this, work with close(), but we want
			// to survive not connecting to the facilitator (although perhaps we should
			// be able to indicate whether we're connected...)
			in = nil;
			out = nil;
			break;
        }
    }
}

/*
 * Send given performative down the output side of this KQMLStream.
 *
 * This function is a version of the very simple trlibSendPerformative
 * function. It would be more elegant these days to have the KQML data
 * types print themselves (like the Java version does). But the C code
 * is *so not* object-oriented.
 *
 * Hmm... NSOutputStream works on some kind of notification basi
 * when it's ready to receive data. Sounds like a drag, or at least more
 * work than I want to do write now (manage buffering for async formatted
 * output). The sample code from Apple (CocoaEcho) blindly writes data
 * without worrying whether it all got written or not. So I think I'll
 * try that approach first. ;-)
 */
- (void)sendMessage:(KQMLPerformative*)perf {
	if (out != nil) {
		[KQMLStream outputPerformative:perf toStream:out];
	} else {
		NSString *str = [KQMLStream performativeToString:perf];
		printf("%s", [str cStringUsingEncoding:NSASCIIStringEncoding]);
	}
}

/*
 * Output given performative to given NSOutputStream.
 * This is simple because KQMLPerforamtive has no internal object structure
 * in the C library version (the `parameters' are just strings).
 */
+ (void)outputPerformative:(KQMLPerformative*)perf toStream:(NSOutputStream*)stream {
    KQMLParameter *q;
    [self outputString: "(" toStream: stream];
    [self outputString: perf->verb toStream: stream];
    for (q=perf->parameters; q != NULL; q=q->next) {
		if (q->key && q->value) {
			[self outputString: " " toStream: stream];
			[self outputString: q->key toStream: stream];
			[self outputString: " " toStream: stream];
			[self outputString: q->value toStream: stream];
		}
    }
    /* This newline is not strictly necessary, but may help some clients */
    [self outputString: ")\n" toStream: stream];
    // Apparently no way to flush, so hope for the best
}

/*
 * Print given C string to given NSOutputStream.
 * One might prefer to implement this as a category on NSOutputStream.
 * Note: We don't get an error (eg., connection refused) until we try
 * to write. So we check here (which we could probably do just the first
 * time).
 */
+ (void)outputString:(char *)str toStream:(NSOutputStream*)stream {
	int len = strlen(str);
	//NSLog(@"KQMLStream:outputString: writing %d bytes", len);
	int written = [stream write:(unsigned char*)str maxLength:len];
	if (written != len) {
		NSLog(@"KQMLStream:outputString: only wrote %d of %d bytes", written, len);
	}
	//NSLog(@"KQMLStream:outputString: wrote %d bytes", written);
}

/*
 * Returns an NSString* representation of given performative.
 */
+ (NSString*)performativeToString:(KQMLPerformative*)perf {
	NSOutputStream *buf = [NSOutputStream outputStreamToMemory];
	[buf open];
	[self outputPerformative:perf toStream:buf];
	NSData *data = [buf propertyForKey: NSStreamDataWrittenToMemoryStreamKey];
	[buf close];
	NSString *str = [[NSString alloc] initWithData: data encoding: NSASCIIStringEncoding];
	return str;
}

@end
