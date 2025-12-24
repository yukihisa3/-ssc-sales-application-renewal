/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷検品システム                     *  ./
/. *   JOB-ID      :    PKE50001                             *  ./
/. *   JOB-NAME    :    （出荷検品）オンラインデータ抽出     *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFIN   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH1  ,STRING*6,VALUE-'SNDDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK1  ,STRING*6,VALUE-'SNDDEK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH2  ,STRING*6,VALUE-'SNDDIN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK2  ,STRING*6,VALUE-'SNDDIK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFIN1  ,STRING*6,VALUE-'SNDFIN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFII2  ,STRING*6,VALUE-'SNDFII'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKWLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SNDDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDDEKF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDDEKFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDFINF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDFINFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE50000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日　　./
    VAR ?P7       ,STRING*1,VALUE-'0'           /.出力区分./
    VAR ?P8       ,STRING*1,VALUE-'0'           /.部門付加./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB
/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P5)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##オンラインデータ抽出指示入力##./
SKE5001I:

    ?STEP :=   'SKE5001I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SKE5001I.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,
                                          ?P7,?P8)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END
    SNDMSG ?P1,TO-XCTL
    SNDMSG ?P2,TO-XCTL
    SNDMSG ?P3,TO-XCTL
    SNDMSG ?P4,TO-XCTL
    SNDMSG ?P5,TO-XCTL
    SNDMSG ?P6,TO-XCTL
    SNDMSG ?P7,TO-XCTL
    SNDMSG ?P8,TO-XCTL

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    IF   ?P7  =  '1' THEN
         ?FILNMH := ?FILNMH1
         ?FILNMK := ?FILNMK1
         ?SNDFIN := ?SNDFIN1
    ELSE
         ?FILNMH := ?FILNMH2
         ?FILNMK := ?FILNMK2
         ?SNDFIN := ?SNDFII2
    END
    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDDENFN :=    %STRING(?SNDDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?SNDDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMK && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDDEKF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDDEKFN :=    %STRING(?SNDDEKF)
    ?MSGX     :=    '## 件数ﾌｧｲﾙ名 = ' && ?SNDDEKFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?SNDFIN && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDFINF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDFINFN :=    %STRING(?SNDFINF)
    ?MSGX     :=    '## 件数ﾌｧｲﾙ名 = ' && ?SNDFINFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##オンラインデータ抽出##./
SKE5012B:

    ?STEP :=   'SKE5012B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SNDDENF,TOFILE-?SNDDENF
    OVRF      FILE-SNDDEKF,TOFILE-?SNDDEKF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SKE5012B.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6,?P8)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##オンラインデータ抽出##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF   ?P7  =  '1' THEN
          FIMPORT FILE-?SNDDENF,PARA-RSTKENPN,UNIT-1
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          FIMPORT FILE-?SNDDEKF,PARA-RSTKENPN,UNIT-2
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          FIMPORT FILE-?SNDFINF,PARA-RSTKENPN,UNIT-5
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          CLRFILE FILE-?SNDFINF
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ELSE
          FIMPORT FILE-?SNDDENF,PARA-RSTKENPN,UNIT-3
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          FIMPORT FILE-?SNDDEKF,PARA-RSTKENPN,UNIT-4
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          FIMPORT FILE-?SNDFINF,PARA-RSTKENPN,UNIT-6
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
          CLRFILE FILE-?SNDFINF
          IF  @PGMEC    ^=   0    THEN
              GOTO ABEND END
    END


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