/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSY00130                             *  ./
/. *   JOB-NAME    :    オンラインデータ抽出                 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'JHSDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?JHSDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHSDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY00130'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

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
PSSY0013:

    ?STEP :=   'PSSY0013'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY0013I.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P5)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END
/.##全倉庫選択の場合は、全倉庫処理へ##./
    IF   ?P4  =  '  '  THEN
          GOTO  SSY0022B
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JHSDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JHSDENFN :=    %STRING(?JHSDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?JHSDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.##オンラインデータ抽出##./
SSY0014B:

    ?STEP :=   'SSY0014B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-JHSDENF,TOFILE-?JHSDENF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0014B.TOKELIB,PARA-(?P1,?P2,?P3,?P4)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    CNVFILE FILE-JHSDEN6A.TOKFLIB,TOFILE-JHSDEN62.TOKFLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-JHSDEN60.TOKFLIB,TOFILE-JHSDEN87.TOKFLIB,
            ADD-@NO,BF-1
    IF  ?P4 = '83' THEN
        CNVFILE FILE-JHSDEN83.TOKFLIB,TOFILE-JHSDEN6M.TOKFLIB,
               ADD-@NO,BF-1
    END
/.##ﾌﾟﾛｸﾞﾗﾑ終了へ##./
    GOTO      RTN

/.##オンラインデータ抽出（全件数）##./
SSY0022B:

    ?STEP :=   'SSY0022B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-JHSDEN01,TOFILE-JHSDEN01.TOKFLIB
    OVRF      FILE-JHSDEN02,TOFILE-JHSDEN6A.TOKFLIB
    OVRF      FILE-JHSDEN03,TOFILE-JHSDEN63.TOKFLIB
    OVRF      FILE-JHSDEN04,TOFILE-JHSDEN84.TOKFLIB
    OVRF      FILE-JHSDEN05,TOFILE-JHSDEN86.TOKFLIB
    OVRF      FILE-JHSDEN06,TOFILE-JHSDEN87.TOKFLIB
    OVRF      FILE-JHSDEN07,TOFILE-JHSDEN90.TOKFLIB
    OVRF      FILE-JHSDEN08,TOFILE-JHSDEN83.TOKFLIB
    OVRF      FILE-JHSDEN09,TOFILE-JHSDEN09.TOKFLIB
    OVRF      FILE-JHSDEN10,TOFILE-JHSDEN10.TOKFLIB
    OVRF      FILE-JHSDEN11,TOFILE-JHSDEN11.TOKFLIB
    OVRF      FILE-JHSDEN12,TOFILE-JHSDEN12.TOKFLIB
    OVRF      FILE-JHSDEN13,TOFILE-JHSDEN13.TOKFLIB
    OVRF      FILE-JHSDEN14,TOFILE-JHSDEN14.TOKFLIB
    OVRF      FILE-JHSDEN15,TOFILE-JHSDEN15.TOKFLIB
    OVRF      FILE-JHSDEN16,TOFILE-JHSDEN16.TOKFLIB
    OVRF      FILE-JHSDEN17,TOFILE-JHSDEN17.TOKFLIB
    OVRF      FILE-JHSDEN18,TOFILE-JHSDEN18.TOKFLIB
    OVRF      FILE-JHSDEN19,TOFILE-JHSDEN19.TOKFLIB
    OVRF      FILE-JHSDEN20,TOFILE-JHSDEN20.TOKFLIB
    CALL      PGM-SSY0022B.TOKELIB,PARA-(?P1,?P2,?P3,?P4)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    CNVFILE FILE-JHSDEN6A.TOKFLIB,TOFILE-JHSDEN62.TOKFLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-JHSDEN60.TOKFLIB,TOFILE-JHSDEN87.TOKFLIB,
            ADD-@NO,BF-1
    IF  ?P4 = '83' THEN
        CNVFILE FILE-JHSDEN83.TOKFLIB,TOFILE-JHSDEN6M.TOKFLIB,
               ADD-@NO,BF-1
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