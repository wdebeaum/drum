/* -*- mode: objc; -*-
 *
 * File: KQMLStream.h
 * Creator: George Ferguson
 * Created: Fri Jan  8 09:10:07 2010
 * Time-stamp: <Fri Jan  8 14:36:37 EST 2010 ferguson>
 */

#include <Foundation/NSStream.h>
#include "KQML/KQML.h"

@protocol KQMLStreamDelegate;

@interface KQMLStream : NSObject <NSStreamDelegate> {
    NSInputStream *in;
    NSOutputStream *out;
	
    KQMLContext *context;
	
    id <KQMLStreamDelegate> _delegate;
}

+ (id)connectedToHost:(NSString*)hostname port:(int)port;
- (BOOL)connectToHost:(NSString*)hostname port:(int)port;
- (void)close;

- (id<KQMLStreamDelegate>)delegate;
- (void)setDelegate:(id<KQMLStreamDelegate>)delegate;

- (void)stream:(NSStream *)stream handleEvent:(NSStreamEvent)eventCode;

- (void)sendMessage:(KQMLPerformative*)perf;

+ (void)outputPerformative:(KQMLPerformative*)perf toStream:(NSOutputStream*)stream;
+ (void)outputString:(char *)str toStream:(NSOutputStream*)stream;
+ (NSString*)performativeToString:(KQMLPerformative*)perf;

@end

@protocol KQMLStreamDelegate <NSObject>
@optional

- (void)KQMLStream:(KQMLStream*)stream receivedMessage:(KQMLPerformative*)msg;
- (void)eofOnKQMLStream:(KQMLStream*)stream;

@end
