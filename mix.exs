defmodule CHAT.Mixfile do
  use Mix.Project

  def project do
    [ app: :mail,
      version: "7.8.0",
      description: "MAIL IPMS MHS Protocol",
      package: package(),
      deps: deps()]
  end

  def application do
    [ mod: {:chat, []}, applications: [:kvs, :syn, :n2o, :mnesia]]
  end

  def package do
    [ files: ~w(include priv src LICENSE mix.exs README.md),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :mail420,
      links: %{"GitHub" => "https://github.com/synrc/mail"}]
  end

  def deps do
    [ {:ex_doc, "~> 0.11", only: :dev},
      {:syn, "~> 2.1.1"},
      {:n2o, "~> 10.8.2"},
      {:kvs, "~> 10.8.2"}]
  end
end
