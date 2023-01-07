{
  arch = "x86_64";
  cabal-lib-version = "3.8.1.0";
  cabal-version = "3.8.1.0";
  compiler-id = "ghc-9.4.3";
  install-plan = [
    {
      depends = [
        "base-4.17.0.0"
      ];
      id = "array-0.5.4.0";
      pkg-name = "array";
      pkg-version = "0.5.4.0";
      type = "pre-existing";
    }
    {
      depends = [
        "ghc-bignum-1.3"
        "ghc-prim-0.9.0"
        "rts"
      ];
      id = "base-4.17.0.0";
      pkg-name = "base";
      pkg-version = "4.17.0.0";
      type = "pre-existing";
    }
    {
      depends = [
        "array-0.5.4.0"
        "base-4.17.0.0"
        "bytestring-0.11.3.1"
        "containers-0.6.6"
      ];
      id = "binary-0.8.9.1";
      pkg-name = "binary";
      pkg-version = "0.8.9.1";
      type = "pre-existing";
    }
    {
      depends = [
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "ghc-prim-0.9.0"
        "template-haskell-2.19.0.0"
      ];
      id = "bytestring-0.11.3.1";
      pkg-name = "bytestring";
      pkg-version = "0.11.3.1";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "bytestring-0.11.3.1"
        "deepseq-1.4.8.0"
        "hashable-1.4.1.0-26698915dce8a7ccfec9b40e311f75d805c8e3bdf3cee6fb658a9e24af3f3a40"
        "text-2.0.1"
      ];
      exe-depends = [];
      flags = {};
      id = "case-insensitive-1.2.1.0-e4738dac29d1b2c0a2efc5e3febccafaca0ed449f81c1d5449f383455d13b9fe";
      pkg-cabal-sha256 = "9dfd3171fc7698cf8d931727d3af3a7b389135b583e46b5adac1f9d2026fff61";
      pkg-name = "case-insensitive";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "296dc17e0c5f3dfb3d82ced83e4c9c44c338ecde749b278b6eae512f1d04e406";
      pkg-version = "1.2.1.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
        "array-0.5.4.0"
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "template-haskell-2.19.0.0"
      ];
      id = "containers-0.6.6";
      pkg-name = "containers";
      pkg-version = "0.6.6";
      type = "pre-existing";
    }
    {
      depends = [
        "array-0.5.4.0"
        "base-4.17.0.0"
        "ghc-prim-0.9.0"
      ];
      id = "deepseq-1.4.8.0";
      pkg-name = "deepseq";
      pkg-version = "1.4.8.0";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "primitive-0.7.4.0-d996e3b44f40eedb09f1aca0a3f64a54e88713420f159784dfe9f09499f6f32b"
      ];
      exe-depends = [];
      flags = {};
      id = "fixed-vector-1.2.1.0-e0d8359ad35f5b7d6b3fdd112f2010aab811c7137a9d90c4e036a30ce7f4f040";
      pkg-cabal-sha256 = "92be7ab0a4bc1d3fc301756cc358f5bd7d29b7c3dbef1c50ee5c75075546892f";
      pkg-name = "fixed-vector";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "aefd2839fc7a7c69f7f9b170b088846be1257e7527ccc84fb002bcea77c4a317";
      pkg-version = "1.2.1.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
        "ghc-prim-0.9.0"
      ];
      id = "ghc-bignum-1.3";
      pkg-name = "ghc-bignum";
      pkg-version = "1.3";
      type = "pre-existing";
    }
    {
      depends = [
        "base-4.17.0.0"
      ];
      id = "ghc-boot-th-9.4.3";
      pkg-name = "ghc-boot-th";
      pkg-version = "9.4.3";
      type = "pre-existing";
    }
    {
      depends = [
        "rts"
      ];
      id = "ghc-prim-0.9.0";
      pkg-name = "ghc-prim";
      pkg-version = "0.9.0";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "bytestring-0.11.3.1"
        "containers-0.6.6"
        "deepseq-1.4.8.0"
        "ghc-bignum-1.3"
        "ghc-prim-0.9.0"
        "text-2.0.1"
      ];
      exe-depends = [];
      flags = {
        containers = true;
        integer-gmp = true;
        random-initial-seed = false;
      };
      id = "hashable-1.4.1.0-26698915dce8a7ccfec9b40e311f75d805c8e3bdf3cee6fb658a9e24af3f3a40";
      pkg-cabal-sha256 = "50b2f002c68fe67730ee7a3cd8607486197dd99b084255005ad51ecd6970a41b";
      pkg-name = "hashable";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "e1b305c280e66ad827edeaedd6933b9fc4174f626882877eab2a08344e665e87";
      pkg-version = "1.4.1.0";
      style = "global";
      type = "configured";
    }
    {
      build-info = "horth/dist-newstyle/build/x86_64-linux/ghc-9.4.3/horth-0.1.0.0/build-info.json";
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "fixed-vector-1.2.1.0-e0d8359ad35f5b7d6b3fdd112f2010aab811c7137a9d90c4e036a30ce7f4f040"
        "megaparsec-9.3.0-3e10bced6a6f7bbecea295d337122e57a46c18b34fe9539ad189f924fad9846a"
        "mtl-2.2.2"
        "text-2.0.1"
        "transformers-0.5.6.2"
        "vector-0.13.0.0-3582f0f7ab9a51628f2668b6b35353e843eba134ee309a04e6a7fd4d1cd819c6"
      ];
      dist-dir = "horth/dist-newstyle/build/x86_64-linux/ghc-9.4.3/horth-0.1.0.0";
      exe-depends = [];
      flags = {};
      id = "horth-0.1.0.0-inplace";
      pkg-name = "horth";
      pkg-src = {
        path = "horth/.";
        type = "local";
      };
      pkg-version = "0.1.0.0";
      style = "local";
      type = "configured";
    }
    {
      bin-file = "horth/dist-newstyle/build/x86_64-linux/ghc-9.4.3/horth-0.1.0.0/x/horth/build/horth/horth";
      build-info = "horth/dist-newstyle/build/x86_64-linux/ghc-9.4.3/horth-0.1.0.0/x/horth/build-info.json";
      component-name = "exe:horth";
      depends = [
        "base-4.17.0.0"
        "horth-0.1.0.0-inplace"
        "text-2.0.1"
      ];
      dist-dir = "horth/dist-newstyle/build/x86_64-linux/ghc-9.4.3/horth-0.1.0.0/x/horth";
      exe-depends = [];
      flags = {};
      id = "horth-0.1.0.0-inplace-horth";
      pkg-name = "horth";
      pkg-src = {
        path = "horth/.";
        type = "local";
      };
      pkg-version = "0.1.0.0";
      style = "local";
      type = "configured";
    }
    {
      component-name = "lib";
      depends = [
        "array-0.5.4.0"
        "base-4.17.0.0"
        "ghc-bignum-1.3"
        "ghc-prim-0.9.0"
      ];
      exe-depends = [];
      flags = {
        check-bounds = false;
        integer-gmp = true;
      };
      id = "integer-logarithms-1.0.3.1-affb9c0c6301ca73d8725b6f2d9d6e6abc69bf041976ed7ba5c262ae31f70235";
      pkg-cabal-sha256 = "d59bfd1d39808217af2b68789d3c0e57cb7199d47405a276060d88fef4f4017d";
      pkg-name = "integer-logarithms";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "9b0a9f9fab609b15cd015865721fb05f744a1bc77ae92fd133872de528bbea7f";
      pkg-version = "1.0.3.1";
      style = "global";
      type = "configured";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "bytestring-0.11.3.1"
        "case-insensitive-1.2.1.0-e4738dac29d1b2c0a2efc5e3febccafaca0ed449f81c1d5449f383455d13b9fe"
        "containers-0.6.6"
        "deepseq-1.4.8.0"
        "mtl-2.2.2"
        "parser-combinators-1.3.0-5936de54086ed9d69747d1df63e46ecaa46fba4e392ad7eff82c2faa6b479ebb"
        "scientific-0.3.7.0-4ac233e6bd059dbb4c8ea0ab8138e52cdcf3d08600997b57e2d7adba4f1dc536"
        "text-2.0.1"
        "transformers-0.5.6.2"
      ];
      exe-depends = [];
      flags = {dev = false;};
      id = "megaparsec-9.3.0-3e10bced6a6f7bbecea295d337122e57a46c18b34fe9539ad189f924fad9846a";
      pkg-cabal-sha256 = "5836aed85cdc87025c2e947040331f2a3cde32911db3b841161c6f3dc4fc5f4f";
      pkg-name = "megaparsec";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "19fcb0846f0e609a242790cae828247e8b70b8739ef24c11f92d52c8862e6005";
      pkg-version = "9.3.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
        "base-4.17.0.0"
        "transformers-0.5.6.2"
      ];
      id = "mtl-2.2.2";
      pkg-name = "mtl";
      pkg-version = "2.2.2";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
      ];
      exe-depends = [];
      flags = {dev = false;};
      id = "parser-combinators-1.3.0-5936de54086ed9d69747d1df63e46ecaa46fba4e392ad7eff82c2faa6b479ebb";
      pkg-cabal-sha256 = "edd54ba56cbae8fadbcceebcfef31b2c70a835e92e5eda41151b939c40647281";
      pkg-name = "parser-combinators";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "9310ef0d49f8a8922acda10b1cded9854cbee04dea717effc6ee5983072e4447";
      pkg-version = "1.3.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "ghc-prim-0.9.0"
      ];
      id = "pretty-1.1.3.6";
      pkg-name = "pretty";
      pkg-version = "1.1.3.6";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "template-haskell-2.19.0.0"
        "transformers-0.5.6.2"
      ];
      exe-depends = [];
      flags = {};
      id = "primitive-0.7.4.0-d996e3b44f40eedb09f1aca0a3f64a54e88713420f159784dfe9f09499f6f32b";
      pkg-cabal-sha256 = "89b88a3e08493b7727fa4089b0692bfbdf7e1e666ef54635f458644eb8358764";
      pkg-name = "primitive";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "5b2d6dc2812eb2f6a115f05fcbe3e723d3aeff7894b012c617e075130581add5";
      pkg-version = "0.7.4.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
      ];
      id = "rts";
      pkg-name = "rts";
      pkg-version = "1.0.2";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "binary-0.8.9.1"
        "bytestring-0.11.3.1"
        "containers-0.6.6"
        "deepseq-1.4.8.0"
        "hashable-1.4.1.0-26698915dce8a7ccfec9b40e311f75d805c8e3bdf3cee6fb658a9e24af3f3a40"
        "integer-logarithms-1.0.3.1-affb9c0c6301ca73d8725b6f2d9d6e6abc69bf041976ed7ba5c262ae31f70235"
        "primitive-0.7.4.0-d996e3b44f40eedb09f1aca0a3f64a54e88713420f159784dfe9f09499f6f32b"
        "template-haskell-2.19.0.0"
        "text-2.0.1"
      ];
      exe-depends = [];
      flags = {
        bytestring-builder = false;
        integer-simple = false;
      };
      id = "scientific-0.3.7.0-4ac233e6bd059dbb4c8ea0ab8138e52cdcf3d08600997b57e2d7adba4f1dc536";
      pkg-cabal-sha256 = "517444c944dad9db8235d7b311d7b9a0839a519ee3178288b5a9606256e0c7d8";
      pkg-name = "scientific";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "a3a121c4b3d68fb8b9f8c709ab012e48f090ed553609247a805ad070d6b343a9";
      pkg-version = "0.3.7.0";
      style = "global";
      type = "configured";
    }
    {
      depends = [
        "base-4.17.0.0"
        "ghc-boot-th-9.4.3"
        "ghc-prim-0.9.0"
        "pretty-1.1.3.6"
      ];
      id = "template-haskell-2.19.0.0";
      pkg-name = "template-haskell";
      pkg-version = "2.19.0.0";
      type = "pre-existing";
    }
    {
      depends = [
        "array-0.5.4.0"
        "base-4.17.0.0"
        "binary-0.8.9.1"
        "bytestring-0.11.3.1"
        "deepseq-1.4.8.0"
        "ghc-prim-0.9.0"
        "template-haskell-2.19.0.0"
      ];
      id = "text-2.0.1";
      pkg-name = "text";
      pkg-version = "2.0.1";
      type = "pre-existing";
    }
    {
      depends = [
        "base-4.17.0.0"
      ];
      id = "transformers-0.5.6.2";
      pkg-name = "transformers";
      pkg-version = "0.5.6.2";
      type = "pre-existing";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "deepseq-1.4.8.0"
        "primitive-0.7.4.0-d996e3b44f40eedb09f1aca0a3f64a54e88713420f159784dfe9f09499f6f32b"
        "vector-stream-0.1.0.0-d04cfbd820bce85a27fca13697d65ad110671112184063799f8e913016d113b0"
      ];
      exe-depends = [];
      flags = {
        boundschecks = true;
        internalchecks = false;
        unsafechecks = false;
        wall = false;
      };
      id = "vector-0.13.0.0-3582f0f7ab9a51628f2668b6b35353e843eba134ee309a04e6a7fd4d1cd819c6";
      pkg-cabal-sha256 = "54528e95501742b99462cc543891f21177e7db34d960ce8e3cbb25afb16a9c89";
      pkg-name = "vector";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "c5d3167d15e12f52e00879ddf304a591672a74e369cc47bc5c7fa1d5a8d15b4f";
      pkg-version = "0.13.0.0";
      style = "global";
      type = "configured";
    }
    {
      component-name = "lib";
      depends = [
        "base-4.17.0.0"
        "ghc-prim-0.9.0"
      ];
      exe-depends = [];
      flags = {};
      id = "vector-stream-0.1.0.0-d04cfbd820bce85a27fca13697d65ad110671112184063799f8e913016d113b0";
      pkg-cabal-sha256 = "09b0f8dc4e51936b9d6b04791f0aa03f7c9759b5fb7140eac8a9461cda1e55a3";
      pkg-name = "vector-stream";
      pkg-src = {
        repo = {
          type = "secure-repo";
          uri = "http://hackage.haskell.org/";
        };
        type = "repo-tar";
      };
      pkg-src-sha256 = "a888210f6467f155090653734be5cc920406a07227e0d3adb59096716fdb806c";
      pkg-version = "0.1.0.0";
      style = "global";
      type = "configured";
    }
  ];
  os = "linux";
}
