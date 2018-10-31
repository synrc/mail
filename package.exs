defmodule ROSTER.Mixfile do
  use Mix.Project

  def project do
    [app: :roster,
     version: "3.4.0",
     description: "ROSTER Messaging Protocol",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:roster, []}]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config", "sys.config", "vm.args"],
     licenses: ["MIT"],
     maintainers: ["Namdak Tonpa"],
     name: :roster,
     links: %{"GitHub" => "https://github.com/synrc/roster"}]
  end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
