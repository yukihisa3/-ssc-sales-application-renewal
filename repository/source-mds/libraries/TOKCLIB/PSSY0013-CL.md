# PSSY0013

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSY0013.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0013                             *  ./
/. *   JOB-NAME    :    オンラインデータ抽出                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0013'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付  ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間  ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先./
    VAR       ?P4       ,STRING*2,VALUE-'00'       /.倉庫      ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


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
    CALL      PGM-SSY0013I.TOKELIB,PARA-(?P1,?P2,?P3,?P4)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END
/.##全倉庫選択の場合は、全倉庫処理へ##./
    IF   ?P4  =  '00'  THEN
          GOTO  SSY0022B
    END

/.##オンラインデータ抽出##./
SSY0014B:

    ?STEP :=   'SSY0014B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    IF ?P4 = '01'  THEN       /.##本社定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN01.TOKFLIB
    END
    IF ?P4 = '62'  THEN       /.##片岡定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN02.TOKFLIB
    END
    IF ?P4 = '63'  THEN       /.##フバサミ定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN03.TOKFLIB
    END
    IF ?P4 = '84'  THEN       /.##鴻巣定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN04.TOKFLIB
    END
    IF ?P4 = '86'  THEN       /.##西尾定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN05.TOKFLIB
    END
    IF ?P4 = '87'  THEN       /.##大和定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN06.TOKFLIB
    END
    IF ?P4 = '90'  THEN       /.##手綱定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN07.TOKFLIB
    END
    IF ?P4 = '83'  THEN       /.##富岡定義##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN08.TOKFLIB
    END
    IF ?P4 = '41'  THEN       /.##福岡営業所##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN41.TOKFLIB
    END
    IF ?P4 = '49'  THEN       /.##大阪営業所##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDEN49.TOKFLIB
    END
    IF ?P4 = 'E1'  THEN       /.##福岡営（ユリックス）##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDENE1.TOKFLIB
    END
    IF ?P4 = 'E9'  THEN       /.##大阪営（大和倉庫）##./
        OVRF      FILE-JHSDENF,TOFILE-JHSDENE9.TOKFLIB
    END
    CALL      PGM-SSY0014B.TOKELIB,PARA-(?P1,?P2,?P3,?P4)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
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
    OVRF      FILE-JHSDEN01,TOFILE-JHSDEN01.TOKFLIB
    OVRF      FILE-JHSDEN02,TOFILE-JHSDEN02.TOKFLIB
    OVRF      FILE-JHSDEN03,TOFILE-JHSDEN03.TOKFLIB
    OVRF      FILE-JHSDEN04,TOFILE-JHSDEN04.TOKFLIB
    OVRF      FILE-JHSDEN05,TOFILE-JHSDEN05.TOKFLIB
    OVRF      FILE-JHSDEN06,TOFILE-JHSDEN06.TOKFLIB
    OVRF      FILE-JHSDEN07,TOFILE-JHSDEN07.TOKFLIB
    OVRF      FILE-JHSDEN08,TOFILE-JHSDEN08.TOKFLIB
    OVRF      FILE-JHSDEN09,TOFILE-JHSDEN41.TOKFLIB
    OVRF      FILE-JHSDEN10,TOFILE-JHSDEN49.TOKFLIB
    OVRF      FILE-JHSDEN11,TOFILE-JHSDENE1.TOKFLIB
    OVRF      FILE-JHSDEN12,TOFILE-JHSDENE9.TOKFLIB
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

```
