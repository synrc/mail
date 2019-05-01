#include <websocketpp/config/asio_client.hpp>
#include <websocketpp/client.hpp>

#include <imgui.h>

#include <iostream>
#include <string>
#include <thread>

#include <cstdio>
#include "asn/Msg.h"

using namespace std;

typedef websocketpp::client<websocketpp::config::asio_tls_client> client;
typedef websocketpp::lib::shared_ptr<websocketpp::lib::asio::ssl::context> context_ptr;

using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;

typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

extern void DoAddLog(string);

struct p {
  client *c;
  websocketpp::connection_hdl hdl;
};

OCTET_STRING_t octet_string(string t) {
  OCTET_STRING_t s = {0};
  s.buf = NULL;
  OCTET_STRING_fromString(&s, t.c_str());
  return s;
}

static int send_to_client(const void *data, size_t size, void *pp) {
  websocketpp::lib::error_code ec;
  ((p *)pp)->c->send(((p *)pp)->hdl, string((char*)data, size), websocketpp::frame::opcode::binary, ec);
  return !!ec;
}

void on_open(client* c, websocketpp::connection_hdl hdl) {
  MUC muc = {octet_string("hello")};
  p p{c,hdl};
  der_encode(&asn_DEF_MUC, &muc, send_to_client, &p);
  xer_fprint(stdout, &asn_DEF_MUC, &muc);

  DoAddLog("connection opened.");
}

// This message handler will be invoked once for each incoming message. It
// prints the message and then sends a copy of the message back to the server.
void on_message(client* c, websocketpp::connection_hdl hdl, message_ptr msg) {
  std::cout << "on_message called with hdl: " << hdl.lock().get()
            << " and message: " << msg->get_payload()
            << std::endl;

#if 0
  websocketpp::lib::error_code ec;
  c->send(hdl, msg->get_payload(), msg->get_opcode(), ec);
  if (ec) {
    std::cout << "Echo failed because: " << ec.message() << std::endl;
  }
#endif
}


context_ptr on_tls_init(const char * hostname, websocketpp::connection_hdl) {
    context_ptr ctx = websocketpp::lib::make_shared<asio::ssl::context>(asio::ssl::context::sslv23);

    ctx->set_options(asio::ssl::context::default_workarounds |
                     asio::ssl::context::no_sslv2 |
                     asio::ssl::context::no_sslv3 |
                     asio::ssl::context::single_dh_use);

    ctx->set_verify_mode(asio::ssl::verify_none);
    return ctx;
}


void wsclient(string uri) {
  client c;

  try {
    // Set logging to be pretty verbose (everything except message payloads)
    c.set_access_channels(websocketpp::log::alevel::all);
    c.clear_access_channels(websocketpp::log::alevel::frame_payload);

    // Initialize ASIO
    c.init_asio();

    c.set_tls_init_handler(bind(&on_tls_init, "hostnamepls", ::_1));
    c.set_open_handler(bind(&on_open,&c,::_1));
    c.set_message_handler(bind(&on_message,&c,::_1,::_2));

    websocketpp::lib::error_code ec;
    client::connection_ptr con = c.get_connection(uri, ec);
    if (ec) {
      std::cout << "could not create connection because: " << ec.message() << std::endl;
    }

    // Note that connect here only requests a connection. No network messages are
    // exchanged until the event loop starts running in the next line.
    c.connect(con);

    // Start the ASIO io_service run loop
    // this will cause a single connection to be made to the server. c.run()
    // will exit when this connection is closed.
    c.run();
  } catch (websocketpp::exception const & e) {
    std::cout << e.what() << std::endl;
  }

  DoAddLog("client aborted.");
}

extern int gui(int, char *[]);

int main(int argc, char* argv[]) {

  std::string uri = "wss://192.168.1.18:8042";

  if (argc == 2) {
    uri = argv[1];
  }

  thread client(wsclient, uri);
  client.detach();

  return gui(argc, argv);
}


// vim: sts=2 sw=2 et
