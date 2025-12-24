/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    営業所データ連携                     *  ./
/. *   JOB-ID      :    PEG00100                             *  ./
/. *   JOB-NAME    :    オンラインデータ抽出                 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?SPATHNM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?SNODENM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'EGYDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?TOJITH   ,STRING*6,VALUE-'TOJITU'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?JYUSNH   ,STRING*6,VALUE-'JYUSIN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SPATHH   ,STRING*5,VALUE-'IHPAT'       /.ﾊﾟｽ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNODEH   ,STRING*5,VALUE-'IHNOD'       /.ﾉｰﾄﾞ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SPATHID  ,NAME                         /.ﾊﾟｽ名名前型./
    VAR ?SNODEID  ,NAME                         /.ﾉｰﾄﾞ名名前型./
    VAR ?JHSDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHSDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?TOJITUF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?TOJITUFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?JYUSINF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JYUSINFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SCOMNM   ,NAME!MOD                     /.ﾊﾟｽ.ﾉｰﾄﾞ./
    VAR ?SCOMNMF  ,STRING*15                    /.ﾊﾟｽ.ﾉｰﾄﾞ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PEG00100'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫      ./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##オンラインデータ抽出指示入力##./
SEG0010I:

    ?STEP :=   'SEG0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SEG0010I.TOKELIB,PARA-(?P1,?P2,?P3,?P4)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 営業所ｺｰﾄﾞ   = ' && ?P4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JHSDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JHSDENFN :=    %STRING(?JHSDENF)
    ?MSGX     :=    '## 営業所ﾌｧｲﾙ名 = ' && ?JHSDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?TOJITH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?TOJITUF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?TOJITUFN :=    %STRING(?TOJITUF)
    ?MSGX     :=    '## 当日ﾌｧｲﾙ名   = ' && ?TOJITUFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?JYUSNH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JYUSINF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JYUSINFN :=    %STRING(?JYUSINF)
    ?MSGX     :=    '## 件数ﾌｧｲﾙ名   = ' && ?JYUSINFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##オンラインデータ抽出##./
SEG0020B:

    ?STEP :=   'SEG0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-JHSDENF,TOFILE-?JHSDENF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SEG0020B.TOKELIB,PARA-(?P1,?P2,?P3)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##当日スケジュールマスタ抽出##./
SEG0030B:

    ?STEP :=   'SEG0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
    OVRF      FILE-JHMTJSWK,TOFILE-?TOJITUF
    CALL      PGM-SEG0030B.TOKELIB,PARA-(?P1,?P2,?P3)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##受信件数マスタ抽出##./
SEG0040B:

    ?STEP :=   'SEG0040B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JHMKENL1,TOFILE-JHMKENL1.TOKFLIB
    OVRF      FILE-JHMKENWK,TOFILE-?JYUSINF
    CALL      PGM-SEG0040B.TOKELIB,PARA-(?P1,?P2,?P3)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##パス名＋ノード名取得##./

    ?SPATHNM  :=    ?SPATHH && ?P4
    ?SNODENM  :=    ?SNODEH && ?P4
    ?SPATHID  :=    %NAME(?SPATHNM)
    ?SNODEID  :=    %NAME(?SNODENM)
    ?SCOMNM   :=    %NCAT(?SPATHID,?SNODEID)
    ?SCOMNMF  :=    %STRING(?SCOMNM)
    ?MSGX     :=    '## 送信ﾊﾟｽ名    = ' && ?SCOMNMF && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##オンラインデータ送信##./
SNDFILE1:

    ?STEP :=   'SNDFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## ｵﾝﾗｲﾝﾃﾞｰﾀ送信中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /. SNDFILE COM-?SCOMNM,SFILE-?JHSDENF,DFILE-'EGYDENF',RL-1020,BF-1
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## ｵﾝﾗｲﾝﾃﾞｰﾀ送信異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## ｵﾝﾗｲﾝﾃﾞｰﾀ送信完了 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
   ./
/.##当日スケジュールマスタ送信##./
SNDFILE2:

    ?STEP :=   'SNDFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## ﾄｳｼﾞﾂﾃﾞｰﾀ送信中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /. SNDFILE COM-?SCOMNM,SFILE-?TOJITUF,DFILE-'TOJITUF',RL-100,BF-1
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## ﾄｳｼﾞﾂﾃﾞｰﾀ送信異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## ﾄｳｼﾞﾂﾃﾞｰﾀ送信完了 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
   ./
/.##受信件数マスタ送信##./
SNDFILE3:

    ?STEP :=   'SNDFILE3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## ｼﾞｭｼﾝﾃﾞｰﾀ送信中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /. SNDFILE COM-?SCOMNM,SFILE-?JYUSINF,DFILE-'JYUSINF',RL-100,BF-1
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## ｼﾞｭｼﾝﾃﾞｰﾀ送信異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## ｼﾞｭｼﾝﾃﾞｰﾀ送信完了 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    ./

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
