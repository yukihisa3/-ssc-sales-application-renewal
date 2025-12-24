/.**************************************************************
  *                                                            *
  *                   ＣＶＣＳ取引先集信処理                   *
  *                          PEOSCVC1                          *
  *                                                            *
  *     呼び出し元CL→PEOSTEL1(ＥＯＳ受信指示画面)             *
  *                                                            *
  *     処理概要  _集信メニューの表示                         *
  *               _CVCSの取引先用集信PG実行前準備             *
  *               _CVCSの取引先用集信PGの実行 (CVCS101)       *
  *               _CVCS管理ファイル創成                       *
  *               _集信データ編集PGの実行前準備               *
  *               _集信データ編集PGの実行                     *
  *               _集信データ編集PGの実行前準備               *
  *               _マスタチェックPGの実行                     *
  *               _エラーファイルの繰越       (SETPF)         *
  *               _EOS受信履歴ファイル作成    (OTXX730)       *
  *               _回線制御ファイルのクリア   (OTXX720)       *
  *                                                            *
  **************************************************************./
   PGM  (P1-?OSY,P2-?PID,P3-?FID,P4-?SFID,P5-?SLIB,P6-?JBID,P7-?KPG,
         P8-?JUN,P9-?CVCPAR,P10-?TOK,P11-?CHID,P12-?ERID,P13-?TNM,
         P14-?ECD,P15-?QSTR)
/.****************./
/.* PASSWORD ﾊﾟﾗ *
/.****************./
   PARA    ?PASS   ,STRING*13,IN,VALUE-'             ' /.ﾊﾟｽﾜｰﾄﾞ./
   PARA    ?HENID  ,STRING*8,IN,VALUE-'        ' /.ﾍﾝｶﾝID./
   PARA    ?SEN    ,STRING*1,IN,VALUE-' '        /.ｶｲｾﾝ./
   PARA    ?SENY   ,STRING*1,IN,VALUE-' '        /.ｶｲｾﾝﾕｳｾﾝ./
   PARA    ?SENLIB ,STRING*8,IN,VALUE-'        ' /.ｶｲｾﾝﾗｲﾌﾞﾗﾘ./
   PARA    ?SYUSINF,STRING*8,IN,VALUE-'        ' /.ｼｭｳｼﾝﾌｧｲﾙ./
   PARA    ?JDATE  ,STRING*8,IN,VALUE-'        ' /.ｼﾞｭｼﾝﾋﾂﾞｹ./
   PARA    ?JTIME  ,STRING*4,IN,VALUE-'        ' /.ｼﾞｭｼﾝｼﾞｶﾝ./
   PARA    ?JTOKCD ,STRING*8,IN,VALUE-'        ' /.ﾄﾘﾋｷｻｷｺｰﾄﾞ./
/.**************./
/.*   OUTQSEL  *./
/.**************./
   VAR     ?PGMA   ,STRING*24!MIXED
   VAR     ?PGMB   ,STRING*24!MIXED
   VAR     ?QNM    ,NAME
/.**名前型変換用**./
   VAR     ?LIBN   ,NAME                        /.ライブラリ用./
   VAR     ?FILN   ,NAME                        /.ファイル用./
   VAR     ?FILLN  ,NAME!MOD                    /.ファイル用拡張./
   VAR     ?PGN    ,NAME                        /.ＰＧ用./
   VAR     ?PGLN   ,NAME!MOD                    /.ＰＧ用拡張./
   VAR     ?TFILN  ,NAME                        /.TOファイル用./
   VAR     ?TFILLN ,NAME!MOD                    /.TOファイル用拡張./
/.**************
  * CL管理 ﾊﾟﾗ *
  **************./
   VAR     ?RC     ,INTEGER
   VAR     ?PGID   ,STRING*10
   VAR     ?PGNM   ,STRING*24!MIXED
   VAR     ?MSG    ,STRING*40
   VAR     ?WS     ,NAME
   VAR     ?VTAM1  ,NAME,VALUE-DICVCS01      /.VTAM./
   VAR     ?ISF1   ,NAME,VALUE-CVCSI001      /.管理M./
   VAR     ?DLIB   ,NAME,VALUE-NAKAFLIB     /.伝票D,中間D格納LIB./
   VAR     ?JBNM   ,NAME                   /.エラーリスト投入JOB用./
   VAR     ?JBST   ,STRING*8,VALUE-'        ' /.リスト投入JOBWK./
   VAR     ?RDCNT  ,STRING*6,VALUE-'      '  /.集信D集信件数./
   VAR     ?SDATEY ,STRING*7,VALUE-'       ' /.システム日付取得./
   VAR     ?SDATE  ,STRING*6,VALUE-'      '  /.システム日付取得./
   VAR     ?STIMES ,STRING*6,VALUE-'      '  /.集信開始時刻取得./
   VAR     ?STIMEE ,STRING*6,VALUE-'      '  /.集信終了時刻取得./
   VAR     ?JFLG   ,STRING*1,VALUE-' '       /.受信ＦＬＧ./
   VAR     ?PST    ,STRING*11,VALUE-'           '  /.数字→文字変換./
   VAR     ?PGST   ,STRING*2,VALUE-'  '      /.PGステータスSTRING./

   VAR     ?JSTIME ,STRING*6,VALUE-'      '  /.受信開始時間./
   VAR     ?JSTHH  ,STRING*2,VALUE-'  '      /.受信開始日　時./
   VAR     ?JSTMM  ,STRING*2,VALUE-'  '      /.受信開始日　分./
   VAR     ?MSGR   ,STRING*99
   VAR     ?KAISU  ,INTEGER

DATESV: /.ﾜｰｸｽﾃｰｼｮﾝID､ｼﾞｭｼﾝｶｲｼﾋﾂﾞｹ､ｼﾞｭｼﾝｶｲｼｼﾞｶﾝ 取得./
   ?WS     :=   @ORGWS                      /.実行ＷＳ名称取得./
   ?SDATEY :=   @SDATEY                     /.システム日付取得./
   ?SDATE  :=   %SBSTR(?SDATEY,1,6)         /.システム日付取得./
   ?STIMES :=   @STIME                      /.受信開始時刻取得./
INIT1001:
/.## CVCS取引先集信前準備作業 ##./
/.## VTAM置換 ##./
   ?FILN  := ?VTAM1
   ?LIBN  := %NAME(?SENLIB)
   ?FILLN := %NCAT(?FILN,?LIBN)
   OVRVTMF FILE-DICVCS01,AFILE-DOCVCS01,TOFILE-?FILLN
/.## 管理M置換 ##./
   ?FILN  := ?ISF1
   ?LIBN  := %NAME(?SENLIB)
   ?FILLN := %NCAT(?FILN,?LIBN)
   OVRISF  FILE-CVCSI001,TOFILE-?FILLN
/.## 集信F置換 ##./
   ?FILN  := %NAME(?SYUSINF)
   ?LIBN  := %NAME(?SENLIB)
   ?FILLN := %NCAT(?FILN,?LIBN)
   OVRF    FILE-CVCSG001,TOFILE-?FILLN
/.## リダイヤル回数クリア ##./
   ?KAISU := 1
/.## 受信開始時間取得 ##./
   ?JSTIME :=   @STIME                      /.受信開始時刻取得./
   ?JSTHH  :=   %SBSTR(?JSTIME,1,2)
   ?JSTMM  :=   %SBSTR(?JSTIME,3,2)
                                            /.受信開始時間表示./
   ?MSGR   :=   '## ｼﾞｭｼﾝ START => ' && ?JSTHH && ':' && ?JSTMM
   SNDMSG MSG^?MSGR,TO-XCTL.@ORGPROF,JLOG-@YES
                                            /.受信パラメタ表示./
   ?MSGR   :=   '## ｼﾞｭｼﾝ ﾊﾟﾗﾒﾀ => ' && ?PASS
   SNDMSG MSG-?MSGR,TO-XCTL.@ORGPROF,JLOG-@YES
CVCS1001:                                   /.CVCS受信処理起動./
   CALL CVCS1001.CVCSLIB,PARA-(?CVCPAR)
   ?RC    := @PGMEC
   ?PGMES := @PGMES
   SNDMSG MSG-?PGMES,TOWS-@ORGWS
   IF ?RC ^= 0  THEN
      ?FILN  := %NAME(?SYUSINF)                  /.ｼｭｳｼﾝﾌｧｲﾙｸﾘｱ./
      ?LIBN  := %NAME(?SENLIB)
      ?FILLN := %NCAT(?FILN,?LIBN)
      CLRFILE FILE-?FILLN
      IF  ?RC = 4095 && ?KAISU ^= 5 THEN
          MSGR := '## ﾘﾀﾞｲﾔﾙ ﾀｲｷﾁｭｳ ' && ?KAISU && ' ##'
          SNDMSG MSG-?MSGR,TO-XCTL.@ORGPROF,JLOG-@YES
          ?KAISU := ?KAISU + 1
          GOTO   CVCS1001
      ELSE
          MSGR := '## ﾄﾘﾋｷｻｷ ﾊﾅｼﾁｭｳ ##'
          SNDMSG MSG-?MSGR,TO-XCTL.@ORGPROF,JLOG-@YES
      END
   END
   ?STIMEE :=   @STIME                      /.受信終了時刻取得./
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*         受信  ＥＮＤ           *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
/.## 受信正常終了 結果更新 ##'
   CALL   PGM-SCV0100B.TOKCLIB,PARA-('00')
/.## 受信データ送信準備 ##./
/.## 集信F置換 ##./
   ?FILN  := %NAME(?SYUSINF)
   ?LIBN  := %NAME(?SENLIB)
   ?FILLN := %NCAT(?FILN,?LIBN)
   IF  ?RLFLG =  '2048'  THEN
        SNDFILE COM-PCPATH02.PCNODE02,SFILE-?FILLN,DFILE-'CVCSINS',
                RL-2048,BF-1


   SNDFILE COM-KGPATH02.KGNODE02,SFILE-AAA.@LIBL,DFILE-'AAA',
        RL-2048,BF-1
   SNDFILE
       MENU IEOSDE2
   END
   IF  ?JBID = 'JB3'  THEN
       MENU IEOSDE3
   END
   IF  ?JBID = 'JB4'  THEN
       MENU IEOSDE4
   END
   IF  ?JBID = 'JB5'  THEN
       MENU IEOSDE5
   END
CVCSKAN:
/.*************************************************
  * _管理ファイル創成                            *
  * 処理概要:現在使用回線の管理マスタに対して     *
  *          再作成を行う.(ﾗｲﾌﾞﾗﾘ管理されている)  *
  * 実行プログラム名は呼び出し元のONXX710で決定済 *
  *************************************************./
   OVRPRTF FILE-XU04LP,OUTQ-ZCVCSQ
   ?PGN := %NAME(?KPG)
   CALL ?PGN
   DLTSPLF FILE-*,SPLQ-ZCVCSQ
INIT010:
/.******************************
  * _伝票データ編集前準備作業 *
  ******************************./
/.**************************
  * 特定取引先必須項目入力 *
  *     白銀屋＆トップ     *
  * 95/12/06プルミエール   *
  *99/02/01よりトップは未使用
  **************************./
/. トップ ./
/. IF ?RC = 0 & ?PID = 'ONJE010B' THEN
      CALL ONXX740,(?JBID,?TNM,?NOHINB)
   END ./
/. 白銀屋 ./
   IF ?RC = 0 & ?PID = 'ONJE010E' THEN
      CALL ONXX740,(?JBID,?TNM,?NOHINB)
   END
/. プルミエール ./
   IF ?RC = 0 & ?PID = 'ONJE010K' THEN
      CALL ONXX740,(?JBID,?TNM,?NOHINB)
   END
/.*************
  * 集信F置換 *
  *************./
   ?FILN  := %NAME(?SFID)
   ?LIBN  := %NAME(?SLIB)
   ?FILLN := %NCAT(?FILN,?LIBN)
   OVRF    FILE-SYUSINF,TOFILE-?FILLN
/.******************
  * 伝票データ置換 *
  ******************./
   ?FILN  := %NAME(?FID)
   ?FILLN := %NCAT(?FILN,?DLIB)
   OVRLF   FILE-NDDENPL1,TOFILE-?FILLN
/.**********************
  * 伝票中間データ置換 *
  **********************./
   ?FILN  := %NAME(?CHID)
   ?FILLN := %NCAT(?FILN,?DLIB)
   OVRPF   FILE-NWDENPP,TOFILE-?FILLN
/.**********************
  * EOSエラーデータ置換 *
  **********************./
   ?FILN  := %NAME(?ERID)
   ?FILLN := %NCAT(?FILN,?DLIB)
   OVRPF   FILE-ERRFILE,TOFILE-?FILLN
ONJE010:
/.*************************************************
  * _伝票データ編集                              *
  * 処理概要:集信ファイルを読み込み伝票データの   *
  *          レイアウトへ変換後存在チェックを行い *
  *          存在しなければ伝票データと中間データ *
  *          へ書き出す.                          *
  * 実行プログラム名は呼び出し元のONJE070で決定済 *
  *************************************************./
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*      データ編集ＳＴＡＲＴ      *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   ?MSG :=    '*       実行JOB名=' && ?JBID && '       *'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   ?MSG :=    '*       受信先名 =' && ?TNM  && '*'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   ?PGN := %NAME(?PID)
/. トップ ./
/. 99/02/01よりトップは未使用  ./
/. IF ?PID = 'ONJE010B' THEN
      CALL ?PGN,(?NOHINB,?RDCNT)
      ?RC    := @PGMEC
      GOTO   PG010END
   END ./
/. 白銀屋 ./
   IF ?PID = 'ONJE010E' THEN
      CALL ?PGN,(?NOHINB,'56041837',?RDCNT)
      ?RC    := @PGMEC
      GOTO   PG010END
   END
/. プルミエール ./
   IF ?PID = 'ONJE010K' THEN
      CALL ?PGN,(?NOHINB,?RDCNT)
      ?RC    := @PGMEC
      GOTO   PG010END
   END
/. 埼玉生協 ./
   IF ?PID = 'PNJE010F' THEN
      CALL ?PGN,(?CVCPAR,?SFID,?SLIB,?JBID,?KPG,?TNM,?RC,?NG,?RDCNT)
      GOTO   PG010END
   END
/. 原信 ./
   IF ?PID = 'PNJE010X' THEN
      CALL ?PGN,(?RC,?RDCNT)
      GOTO   PG010END
   END
/. フレッセイ ./
   IF ?PID = 'PNJE510G' THEN
      CALL ?PGN,(?RC,?RDCNT)
      GOTO   PG010END
   END
/. 通　常 ./
   CALL ?PGN,(?RDCNT)
   ?RC    := @PGMEC
PG010END:
/. 二重エラーリスト ./
   IF  ?RC = 4002 & ?NG = 'OK' THEN
       CALL ONJE030
     ELSE
       IF  ?RC ^= 0  THEN
           ?RC    := @PGMEC
           ?PGMES := @PGMES
           SNDMSG MSG-?PGMES,TOWS-@ORGWS
           GOTO   ABEND
       END
   END
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*      データ編集  ＥＮＤ        *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   ?MSG :=    '*       実行JOB名=' && ?JBID && '       *'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   ?MSG :=    '*       受信先名 =' && ?TNM  && '*'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
INIT040:
/.******************************
  * _マスタチェック前準備作業 *
  ******************************./
/.**********************
  * 伝票中間データ置換 *
  **********************./
   ?FILN  := %NAME(?CHID)
   ?FILLN := %NCAT(?FILN,?DLIB)
   OVRPF   FILE-NWDENPP,TOFILE-?FILLN
/.***********************
  * EOSエラーデータ置換 *
  ***********************./
   ?FILN  := %NAME(?ERID)
   ?FILLN := %NCAT(?FILN,?DLIB)
   OVRPF   FILE-ERRFILE,TOFILE-?FILLN
/.**************************************
  * 特定受信先予備取引先コードセット   *
  * 95/04/04現在は埼玉生協と県民生協   *
  * 95/04/14にジャスコにメガマート追加 *
  * 97/04/23にドラッグイイズカ追加     *
  * 埼玉生協='931039  ' , '990000  '   *
  * 県民生協='004212  ' , '002914  '   *
  * ジャスコ='05387   ' , '55721   '   *
  * ドラッグ ?TOK='0242'               *
  **************************************./
   IF  ?ECD   = '931039  ' THEN
       ?ECD1 := '990000  '
   END
   IF  ?ECD   = '004212  ' THEN
       ?ECD1 := '002914  '
   END
   IF  ?ECD   = '05387   ' THEN
       ?ECD1 := '55721   '
   END
   IF  ?TOK   = '0242' THEN
       ?ECD1 := '008200  '
   END
ONJE040:
/.*************************************************
  * _マスタチェック                              *
  * 処理概要：各種マスタとデータのチェックを行う. *
  * この時,正常データはVLD001に書き出し,ERRデータ *
  * はエラーファイルに書き出す(PGSTATUSに4002).   *
  * 正常データは単価や粗利のチェックを行い規定外  *
  * の場合はアンマッチリストを出力する.           *
  * PGSTATUSが4002の時はエラーリストの出力ジョブ  *
  * をBGJOBとして投入する.                        *
  *      ** 受取PARA-対象取引先コード1～3 **      *
  * 受け取ったPARAで対象データチェックを行う.     *
  * 対象取引先コード1がALL"9"の時は全件対象とする *
  *************************************************./
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*    マスタチェックＳＴＡＲＴ    *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   ?MSG :=    '*       実行JOB名=' && ?JBID && '       *'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   ?MSG :=    '*       受信先名 =' && ?TNM  && '*'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   CALL ONJE040,(?ECD,?ECD1,?ECD2,?JBID)
   ?RC := @PGMEC
   IF  ?RC = 4002 THEN
       ?JBST := 'BG' && %SBSTR(?JBID,1,6)
       ?JBNM :=  %NAME(?JBST)
       ?MSG :=    '**マスタチェックエラー発生**'
       SNDMSG MSG-?MSG,TOWS-@ORGWS
       ?MSG :=    '**投入JOB名 ' && ?JBST && '     **'
       SNDMSG MSG-?MSG,TOWS-@ORGWS
       SBMJOB JOB-?JBNM,JOBD-BGJDCVC1.XUCL,PGM-PNJE050,
              PARA-(?ERID,?TNM,?JBID,?ECD,?ECD1,?ECD2,?QSTR)
   END
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*    マスタチェック  ＥＮＤ      *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   ?MSG :=    '*       実行JOB名=' && ?JBID && '       *'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   ?MSG :=    '*       受信先名 =' && ?TNM  && '*'
   SNDMSG MSG-?MSG,TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
SETPF:
/.*************************************************
  * _エラーファイル繰越                          *
  * 処理概要:マスタチェックで書き出されたエラー   *
  *          ファイルを伝票中間データへSETPFを行う*
  *************************************************./
/.*******************
  * EOSエラーデータ *
  *******************./
   ?FILN  := %NAME(?ERID)
   ?FILLN := %NCAT(?FILN,?DLIB)
/.******************
  * 伝票中間データ *
  ******************./
   ?TFILN  := %NAME(?CHID)
   ?TFILLN := %NCAT(?TFILN,?DLIB)
   SETPF FILE-?FILLN,TOFILE-?TFILLN,ADD-@NO,ACTCHK-@NO
   ?RC := @PGMEC
ONXX820:
/.****************************************
  * ＢＧＪＯＢの終了待合せ               *
  * ONJE040でBGJOBのVLD001へ終了時に     *
  * 制御ファイルの識別区分"E"で作成後,   *
  * VLD001へも作成する.                  *
  * BGJOBのONXX020でVLD READ時に         *
  * このレコードだったら制御ファイルの   *
  * レコードを削除する.                  *
  * ONXX820は常にこの制御ファイルレコード*
  * の存在チェックを行い,存在しなくなる  *
  * までループして処理の終了を待合せる.  *
  ****************************************./
   ?PGNM :=   '　ＢＧ終了待合せ中　'     /.ＰＧ漢字名./
   ?PGID := 'ONXX820   '               /.ＰＧＩＤ　./
   SNDMSG MSG-'********************',TOWS-@ORGWS
   SNDMSG MSG-?JBID,TOWS-@ORGWS
   SNDMSG MSG-?PGNM,TOWS-@ORGWS
   SNDMSG MSG-'しばらくお待ち下さい',TOWS-@ORGWS
   SNDMSG MSG-'********************',TOWS-@ORGWS
   CALL ONXX820,('E',?JBID)
   ?RC  :=   @PGMEC
   IF  ?RC    ^= 0
     THEN
       GOTO   ABEND
   END
   SNDMSG MSG-' ',TOWS-@ORGWS
   SNDMSG MSG-' ',TOWS-@ORGWS
   SNDMSG MSG-' ',TOWS-@ORGWS
   SNDMSG MSG-'********************',TOWS-@ORGWS
   SNDMSG MSG-?JBID,TOWS-@ORGWS
   SNDMSG MSG-'　お待たせしました　',TOWS-@ORGWS
   SNDMSG MSG-'　無事終了しました　',TOWS-@ORGWS
   SNDMSG MSG-'********************',TOWS-@ORGWS
ONXX720:
/.*************************************************
  * _回線制御ファイルクリア                      *
  * 処理概要:現在使用回線のレコードの使用ＦＬＧの *
  *          初期化を行う.                        *
  *          受け取ったパラメータでレコードを     *
  *          読み込む.                            *
  * PARA-回線種別,優先順位                        *
  *************************************************./
   CALL ONXX720,(?OSY,?JUN)
   IF  ?JBID = 'JB2'  THEN
      CTLNSS CTL-ACT,TYPE-LINK,NAME-LINK00
   END
   IF  ?JBID = 'JB3'  THEN
      CTLNSS CTL-ACT,TYPE-LINK,NAME-LINK02
   END
   IF  ?JBID = 'JB4'  THEN
      CTLNSS CTL-ACT,TYPE-LINK,NAME-LINK04
   END
   IF  ?JBID = 'JB5'  THEN
      CTLNSS CTL-ACT,TYPE-LINK,NAME-LINK06
   END
ONXX730:
/.*************************************************
  * _受信履歴ファイル更新                        *
  * 処理概要:受信履歴ファイルの更新を受け取った   *
  *          パラメータより行う                   *
  * PARA-受信日付,受信開始時間,受信終了時間,      *
  *      受信件数,受信フラグ,受信ステータス,      *
  *      受信ＪＯＢ名,得意先コード                *
  *************************************************./
  CALL ONXX730,(?SDATE,?STIMES,?STIMEE,?RDCNT,?JFLG,?PGST,?JBID,?TOK)
  XFRCTL TOJOBN-NAKAYA
END:
/.   VAR     ?PGN    ,NAME    ./                    /.ＰＧ用./
/.   VAR     ?PGLN   ,NAME!MOD ./                   /.ＰＧ用拡張./
   CALL ONXX980,(?QSTR)
   RETURN
ABEND:
/. ********************95/08/15追加************************ ./
   DSPLOG OUTPUT-@LIST,EDT-@JEF
   CALL ONXX720,(?OSY,?JUN)
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
   SNDMSG MSG-'*       伝票データ編集中         *',TOWS-@ORGWS
   SNDMSG MSG-'*        エラー発生！！          *',TOWS-@ORGWS
   SNDMSG MSG-'*                                *',TOWS-@ORGWS
   SNDMSG MSG-'* ファイルの領域があふれた可能性 *',TOWS-@ORGWS
   SNDMSG MSG-'* があります。ナブまで連絡を！！ *',TOWS-@ORGWS
   SNDMSG MSG-'**********************************',TOWS-@ORGWS
/. ********************95/08/15追加************************ ./
   CALL ONXX980,(?QSTR)
   RETURN
