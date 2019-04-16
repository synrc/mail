defmodule CHAT.Mixfile do
  use Mix.Project

  def project do
    [app: :roster,
     version: "3.4.0",
     description: "CHAT Protocol",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:chat, []}]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config", "sys.config", "vm.args"],
     licenses: ["MIT"],
     maintainers: ["Namdak Tonpa","Vladimir Kirillov"],
     name: :chat,
     links: %{"GitHub" => "https://github.com/synrc/chat"}]
  end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
