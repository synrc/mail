#include <websocketpp/config/asio_client.hpp>
#include <websocketpp/client.hpp>

#include <iostream>
#include <string>

#include <cstdio>
#include "asn/Msg.h"

using namespace std;

typedef websocketpp::client<websocketpp::config::asio_client> client;

using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;

typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

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
}

// This message handler will be invoked once for each incoming message. It
// prints the message and then sends a copy of the message back to the server.
void on_message(client* c, websocketpp::connection_hdl hdl, message_ptr msg) {
  std::cout << "on_message called with hdl: " << hdl.lock().get()
            << " and message: " << msg->get_payload()
            << std::endl;

  websocketpp::lib::error_code ec;
  c->send(hdl, msg->get_payload(), msg->get_opcode(), ec);
  if (ec) {
    std::cout << "Echo failed because: " << ec.message() << std::endl;
  }
}

int main(int argc, char* argv[]) {
  // Create a client endpoint
  client c;

  std::string uri = "ws://localhost:9002";

  if (argc == 2) {
    uri = argv[1];
  }

  try {
    // Set logging to be pretty verbose (everything except message payloads)
    c.set_access_channels(websocketpp::log::alevel::all);
    c.clear_access_channels(websocketpp::log::alevel::frame_payload);

    // Initialize ASIO
    c.init_asio();

    // Register our message handler
    c.set_open_handler(bind(&on_open,&c,::_1));
    c.set_message_handler(bind(&on_message,&c,::_1,::_2));

    websocketpp::lib::error_code ec;
    client::connection_ptr con = c.get_connection(uri, ec);
    if (ec) {
      std::cout << "could not create connection because: " << ec.message() << std::endl;
      return 0;
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
}


// vim: sts=2 sw=2 et
