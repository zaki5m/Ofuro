# Ofuro
副露率を指定することができる麻雀AI「Ofuro」．
<h1>環境</h1>
ocaml >= 5.0.0

<h1>buildとシミュレーション実行方法</h1>
port番号を指定しplayerそれぞれの副露率を入力すると2000局のシミュレーションが開始される．
<pre><code>$ git clone https://github.com/zaki5m/Ofuro.git
$ dune build
$ ./_build/default/mahjong_admin.exe port player1 player2 player3 player4
</pre></code>
