# pykqml trips_module.py (was kqml_module.py)
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to better match the equivalent libraries in other
# languages, and to retain greater compatibility with old versions, by William
# de Beaumont.

import io
import os
import sys
import socket
import logging
from KQML import KQMLReader, KQMLDispatcher
from KQML import KQMLToken, KQMLString, KQMLList, KQMLPerformative
from KQML import StopWaitingSignal

logging.basicConfig()

logger = logging.getLogger(__name__)


# On Windows, sockets need to be created via the msvcrt
# package. We try to import it here and use it if available.
try:
    import msvcrt
    use_msvcrt = True
except ImportError:
    use_msvcrt = False


class TripsModule(object):
    def __init__(self, argv, is_application=False):
        self.DEFAULT_HOST = 'localhost'
        self.DEFAULT_PORT = 6200
        self.MAX_PORT_TRIES = 100
        self.reply_id_counter = 1
        self.argv = argv
        self.is_application = is_application
        self.host = self.DEFAULT_HOST
        self.port = self.DEFAULT_PORT
        self.auto_connect = True
        self.socket = None
        self.name = None
        self.group_name = None
        self.scan_for_port = False
        self.inp =  None
        self.out = None
        self.dispatcher = None
        self.warning_enabled = True
        self.debugging_enabled = False
        self.init()

    def run(self):
        self.dispatcher.run()

    def init(self):
        self.handle_common_parameters()
        if self.auto_connect:
            logger.info('Using socket connection')
            conn = self.connect(self.host, self.port)
            if not conn:
                logger.error('Connection failed')
                self.exit(-1)
            assert self.inp is not None and self.out is not None,\
                "Connection formed but input (%s) and output (%s) not set." % (self.inp, self.out)
        else:
            logger.info('Using stdio connection')
            # pykqml's version of this doesn't actually connect to stdin/out?! -- wdebeaum
            #self.out = io.BytesIO()
            #self.inp = KQMLReader(io.BytesIO())
            if hasattr(sys.stdout, 'buffer'): # python 3
                self.out = sys.stdout.buffer
                self.inp = KQMLReader(sys.stdin.buffer)
            else: # python 2
                self.out = sys.stdout
                self.inp = KQMLReader(sys.stdin)

        self.dispatcher = KQMLDispatcher(self, self.inp, self.name)

        if self.name is not None:
            self.register()

    def subscribe_request(self, req_type):
        msg = KQMLPerformative('subscribe')
        content = KQMLList('request')
        content.append('&key')
        content.set('content', KQMLList.from_string('(%s . *)' % req_type))
        msg.set('content', content)
        self.send(msg)

    def subscribe_tell(self, tell_type):
        msg = KQMLPerformative('subscribe')
        content = KQMLList('tell')
        content.append('&key')
        content.set('content', KQMLList.from_string('(%s . *)' % tell_type))
        msg.set('content', content)
        self.send(msg)

    def get_parameter(self, param_str):
        for i, a in enumerate(self.argv):
            if a == param_str:
                return self.argv[i+1]
        return None

    def handle_common_parameters(self):
        value = self.get_parameter('-connect')
        if value is not None:
            if value.lower() in ('true', 't', 'yes'):
                self.auto_connect = True
            elif value.lower() in ('false', 'nil', 'no'):
                self.auto_connect = False
            else:
                colon = value.find(':')
                if colon > -1:
                    self.host = value[0:colon]
                    self.port = int(value[colon+1:])
                else:
                    self.host = value
                    self.port = self.DEFAULT_PORT

        value = self.get_parameter('-name')
        if value is not None:
            self.name = value

        value = self.get_parameter('-group')
        if value is not None:
            self.group = value

        value = self.get_parameter('-scan')
        if value in ('true', 't', 'yes'):
            self.scan_for_port = True
        else:
            self.scan_for_port = False

        value = self.get_parameter('-debug')
        if value in ('true', 't', 'yes'):
            self.set_debugging_enabled(True)
        else:
            self.set_debugging_enabled(False)

    def connect(self, host=None, startport=None):
        if host is None:
            host = self.host
        if startport is None:
            startport = self.port
        if not self.scan_for_port:
            return self.connect1(host, startport, True)
        else:
            maxtries = self.MAX_PORT_TRIES
            for port in range(startport, startport + maxtries):
                conn = self.connect1(host, port, False)
                if conn:
                    return True
            logger.error('Failed to connect to ' + host + ':' + \
                         startport + '-' + port)
            return False

    def connect1(self, host, port, verbose=True):
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((host, port))
            if hasattr(socket, 'SocketIO'):
                sw = socket.SocketIO(self.socket, 'w')
                self.out = io.BufferedWriter(sw)
                sr = socket.SocketIO(self.socket, 'r')
                self.inp = KQMLReader(io.BufferedReader(sr))
            else:
                sfn = self.socket.makefile().fileno()
                fio = io.FileIO(sfn, mode='w')
                self.out = io.BufferedWriter(fio)
                fio = io.FileIO(sfn, mode='r')
                self.inp = KQMLReader(io.BufferedReader(fio))
            return True
        except socket.error as e:
            if verbose:
                logger.error(e)
            return False

    def register(self):
        if self.name is not None:
            perf = KQMLPerformative('register')
            perf.set('name', self.name)
            if self.group_name is not None:
                try:
                    if self.group_name.startswith('('):
                        perf.sets('group', self.group_name)
                    else:
                        perf.set('group', self.group_name)
                except IOError:
                    logger.error('bad group name: ' + self.group_name)
            self.send(perf)

    def ready(self):
        msg = KQMLPerformative('tell')
        content = KQMLList(['module-status', 'ready'])
        msg.set('content', content)
        self.send(msg)

    def exit(self, n):
        if self.is_application:
            sys.exit(n)
        else:
            if self.dispatcher is not None:
                self.dispatcher.shutdown()
            sys.exit(n)

    def stop_waiting(self):
        raise StopWaitingSignal()

    def receive_eof(self):
        self.exit(0)

    def receive_message_missing_verb(self, msg):
        self.error_reply(msg, 'missing verb in performative')

    def receive_message_missing_content(self, msg):
        self.error_reply(msg, 'missing content in performative')

    def receive_ask_if(self, msg, content):
        self.error_reply(msg, 'unexpected performative: ask-if')

    def receive_ask_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: ask-all')

    def receive_ask_one(self, msg, content):
        self.error_reply(msg, 'unexpected performative: ask-one')

    def receive_stream_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: stream-all')

    def receive_tell(self, msg, content):
        logger.error('unexpected performative: tell')

    def receive_untell(self, msg, content):
        self.error_reply(msg, 'unexpected performative: untell')

    def receive_deny(self, msg, content):
        self.error_reply(msg, 'unexpected performative: deny')

    def receive_insert(self, msg, content):
        self.error_reply(msg, 'unexpected performative: insert')

    def receive_uninsert(self, msg, content):
        self.error_reply(msg, 'unexpected performative: uninsert')

    def receive_delete_one(self, msg, content):
        self.error_reply(msg, 'unexpected performative: delete-one')

    def receive_delete_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: delete-all')

    def receive_undelete(self, msg, content):
        self.error_reply(msg, 'unexpected performative: undelete')

    def receive_achieve(self, msg, content):
        self.error_reply(msg, 'unexpected performative: achieve')

    def receive_advertise(self, msg, content):
        self.error_reply(msg, 'unexpected performative: advertise')

    def receive_unadvertise(self, msg, content):
        self.error_reply(msg, 'unexpected performative: unadvertise')

    def receive_subscribe(self, msg, content):
        self.error_reply(msg, 'unexpected performative: subscribe')

    def receive_standby(self, msg, content):
        self.error_reply(msg, 'unexpected performative: standby')

    def receive_register(self, msg, content):
        self.error_reply(msg, 'unexpected performative: register')

    def receive_forward(self, msg, content):
        self.error_reply(msg, 'unexpected performative: forward')

    def receive_broadcast(self, msg, content):
        self.error_reply(msg, 'unexpected performative: broadcast')

    def receive_transport_address(self, msg, content):
        self.error_reply(msg, 'unexpected performative: transport-address')

    def receive_broker_one(self, msg, content):
        self.error_reply(msg, 'unexpected performative: broker-one')

    def receive_broker_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: broker-all')

    def receive_recommend_one(self, msg, content):
        self.error_reply(msg, 'unexpected performative: recommend-one')

    def receive_recommend_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: recommend-all')

    def receive_recruit_one(self, msg, content):
        self.error_reply(msg, 'unexpected performative: recruit-one')

    def receive_recruit_all(self, msg, content):
        self.error_reply(msg, 'unexpected performative: recruit-all')

    def receive_reply(self, msg, content):
        logger.error(msg, 'unexpected performative: reply')

    def receive_request(self, msg, content):
        self.error_reply(msg, 'unexpected performative: request')

    def receive_eos(self, msg):
        self.error_reply(msg, 'unexpected performative: eos')

    def receive_error(self, msg):
        logger.error('Error received: "%s"' % msg)

    def receive_sorry(self, msg):
        logger.error('unexpected performative: sorry')

    def receive_ready(self, msg):
        logger.error(msg, 'unexpected performative: ready')

    def receive_next(self, msg):
        self.error_reply(msg, 'unexpected performative: next')

    def receive_rest(self, msg):
        self.error_reply(msg, 'unexpected performative: rest')

    def receive_discard(self, msg):
        self.error_reply(msg, 'unexpected performative: discard')

    def receive_unregister(self, msg):
        logger.error(msg, 'unexpected performative: unregister')

    def receive_other_performative(self, msg):
        self.error_reply(msg, 'unexpected performative: ' + str(msg))

    def handle_exception(self, ex):
        logger.error(str(ex))

    def send(self, msg):
        try:
            msg.write(self.out)
        except IOError:
            logger.error('IOError during message sending')
            pass
        self.out.write(b'\n')
        self.out.flush()
        logger.debug(msg.__repr__())

    def send_with_continuation(self, msg, cont):
        reply_id_base = 'IO-'
        if self.name is not None:
            reply_id_base = self.name + '-'
        reply_id = reply_id_base + str(self.reply_id_counter)
        self.reply_id_counter += 1
        msg.append(':reply-with')
        msg.append(reply_id)
        self.dispatcher.add_reply_continuation('%s' % reply_id, cont)
        self.send(msg)

    def reply(self, msg, reply_msg):
        sender = msg.get('sender')
        if sender is not None:
            reply_msg.set('receiver', sender)
        reply_with = msg.get('reply-with')
        if reply_with is not None:
            reply_msg.set('in-reply-to', reply_with)
        self.send(reply_msg)

    def error_reply(self, msg, comment):
        reply_msg = KQMLPerformative('error')
        reply_msg.sets('comment', comment)
        self.reply(msg, reply_msg)

    def error(self, msg):
        sys.stderr.write(msg)

    def warn(self, msg):
        if self.warning_enabled:
            sys.stderr.write(msg)

    def set_warning_enabled(self, enable):
        self.warning_enabled = enable

    def debug(self, msg):
        if self.debugging_enabled:
            sys.stderr.write(msg)

    def set_debugging_enabled(self, enable):
        self.debugging_enabled  = enable


if __name__ == '__main__':
    TripsModule(sys.argv[1:]).run()
