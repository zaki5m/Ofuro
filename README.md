# mahjong_ai
副露率を指定することができる麻雀AI．OCaml5.0.0以上で利用可能．
現在は1半荘単位での実行．
<h1>実行</h1>
test branchのソースコードをダウンロード後，以下を実行．nは使用したいスレッド数．mはseed値を指定(指定しなければ1)．
<pre><code>$ dune build
$ ./_build/default/mahjong_admin.exe n m</pre></code>

