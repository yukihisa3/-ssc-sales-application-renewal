/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    営業所データ連携                     *  ./
/. *   JOB-ID      :    PONLEG01                             *  ./
/. *   JOB-NAME    :    営業所ｵﾝﾗｲﾝﾃﾞｰﾀ抽出－送信－本社起動  *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?HIDUKE  ,STRING*8,VALUE-'        '     /.日付./
    VAR ?JIKAN   ,STRING*4,VALUE-'    '         /.時間./
    VAR ?TOKCD   ,STRING*8,VALUE-'        '     /.取引先CD./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?SPATHNM  ,STRING*8,VALUE-'        '    /.ﾊﾟｽ名合併用./
    VAR ?SNODENM  ,STRING*8,VALUE-'        '    /.ﾊﾟｽ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'EGYDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SPATHH   ,STRING*6,VALUE-'IHPATH'      /.ﾊﾟｽ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNODEH   ,STRING*6,VALUE-'IHNODE'      /.ﾉｰﾄﾞ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SPATHID  ,NAME                         /.ﾊﾟｽ名名前型./
    VAR ?SNODEID  ,NAME                         /.ﾉｰﾄﾞ名名前型./
    VAR ?JHSDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHSDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SCOMNM   ,NAME!MOD                     /.ﾊﾟｽ.ﾉｰﾄﾞ./
    VAR ?SCOMNMF  ,STRING*17                    /.ﾊﾟｽ.ﾉｰﾄﾞ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PONLEG01'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫      ./
    VAR ?SOSIN    ,STRING*2,VALUE-'01'          /.送信ID    ./

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
    CALL      PGM-SEG0010I.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                         ?SOKCD)
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

    ?MSGX := '## 営業所ｺｰﾄﾞ   = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?SOKCD     /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JHSDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JHSDENFN :=    %STRING(?JHSDENF)
    ?MSGX     :=    '## 営業所ﾌｧｲﾙ名 = ' && ?JHSDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##オンラインデータ抽出##./
SEG0020B:

    ?STEP :=   'SEG0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-JHSDENF,TOFILE-?JHSDENF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SEG0020B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##パス名＋ノード名取得##./

    ?SPATHNM  :=    ?SPATHH && ?SOSIN
    ?SNODENM  :=    ?SNODEH && ?SOSIN
    ?SPATHID  :=    %NAME(?SPATHNM)
    ?SNODEID  :=    %NAME(?SNODENM)
    ?SCOMNM   :=    %NCAT(?SPATHID,?SNODEID)
    ?SCOMNMF  :=    %STRING(?SCOMNM)
    ?MSGX     :=    '## 送信ﾊﾟｽ名 = ' && ?SCOMNMF && ' ##'
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

/.##本社ＪＯＢ起動##./
SNDRSJJ1:

    ?STEP :=   'SNDRSJJ1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## 本社JOB起動中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  SNDRSJJ COM-?SCOMNM,DFILE-'PONLEG03'
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## 本社JOB起動異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## 本社JOB起動完了 ##'
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