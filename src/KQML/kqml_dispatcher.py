# pykqml kqml_dispatcher.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to better match the equivalent libraries in other
# languages, and to retain greater compatibility with old versions, by William
# de Beaumont.

import logging
from .kqml_exceptions import StopWaitingSignal


logger = logging.getLogger(__name__)


class KQMLDispatcher(object):
    def __init__(self, rec, inp, agent_name):
        super(KQMLDispatcher, self).__init__()
        self.receiver = rec
        self.reader = inp
        self.reply_continuations = {}
        self.counter = 0
        self.name = 'KQML-Dispatcher-%d' % self.counter
        self.agent_name = agent_name
        self.counter += 1
        self.shutdown_initiated = False

    def run(self):
        try:
            while True:
                msg = self.reader.read_performative()
                self.dispatch_message(msg)
        # This signal allows the dispatcher to stop blocking and return without
        # closing the connection to the socket and exiting
        except StopWaitingSignal:
            return
        except KeyboardInterrupt:
            logger.info('Keyboard interrupt received')
            self.receiver.receive_eof()
        except EOFError:
            logger.info('EOF received')
            self.receiver.receive_eof()
        except IOError as ex:
            if not self.shutdown_initiated:
                self.receiver.handle_exception(ex)
        except ValueError as e:
            logger.error('Value error during reading')
            logger.exception(e)
            return

    def warn(self, msg):
        logger.warning(msg)

    def shutdown(self):
        self.shutdown_initiated = True
        try:
            logger.info('KQML dispatcher shutting down')
            self.reader.close()
        except IOError:
            logger.error('KQML dispatched IOError.')
            pass

    def dispatch_message(self, msg):
        verb = msg.head()
        if verb is None:
            self.receiver.receive_message_missing_verb(msg)
            return
        reply_id_obj = msg.get('in-reply-to')
        if reply_id_obj is not None:
            reply_id = reply_id_obj.string_value().upper()
            try:
                value = self.reply_continuations[reply_id]
                value.receive(msg)
                self.reply_continuations.pop(reply_id, 0)
                return
            except KeyError:
                pass

        vl = verb.lower()
        content = msg.get('content')
        content_msg_types = ['ask-if', 'ask-all', 'ask-one', 'stream-all',
                             'tell', 'untell', 'deny', 'insert', 'uninsert',
                             'delete-one', 'delete-all', 'undelete', 'achieve',
                             'unachieve', 'advertise', 'subscribe', 'standby',
                             'register', 'forward', 'broadcast',
                             'transport-address', 'broker-one', 'broker-all',
                             'recommend-one', 'recommend-all', 'recruit-one',
                             'recruit-all', 'reply', 'request']
        msg_only_types = ['eos', 'error', 'sorry', 'ready', 'next', 'next',
                          'rest', 'discard', 'unregister']

        method_name = 'receive_' + vl.replace('-', '_')
        if vl in content_msg_types:
            if content is None:
                self.receiver.receive_message_missing_content(msg)
                return

            for cmt in content_msg_types:
                if vl == cmt:
                    self.receiver.__getattribute__(method_name)(msg, content)
        elif vl in msg_only_types:
            for cmt in msg_only_types:
                self.receiver.__getattribute__(method_name)(msg)
        else:
            self.receiver.receive_other_performative(msg)

        return

    def add_reply_continuation(self, reply_id, cont):
        self.reply_continuations[reply_id.upper()] = cont
