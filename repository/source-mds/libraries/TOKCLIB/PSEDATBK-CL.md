# PSEDATBK

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSEDATBK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSEDATBK                             *  ./
/. *   JOB-NAME    :    退避処理（伝票データ）               *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSEDATBK'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB

/.  請求締日処理前データ退避確認                                ./
STEP00:

    ?OPR1  :=  '　＃＃＃＃　前回請求締日データ退避処理　＃＃＃＃'
    ?OPR2  :=  '　前回分の請求締日データの退避処理を行ないます。'
    ?OPR3  :=  ''
    ?OPR4  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.   退避処理                                                   ./
TAIHI1:

    ?STEP :=   'TAIHI1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ａ　１／８＃',TO-XCTL
    SAVFILE FILE-SETGKFA.TOKFLIB,TODEV-LTO,REWIND-@BEFORE,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI2:

    ?STEP :=   'TAIHI2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｂ　２／８＃',TO-XCTL
    SAVFILE FILE-SETGKFB.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI3:

    ?STEP :=   'TAIHI3  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｃ　３／８＃',TO-XCTL
    SAVFILE FILE-SETGKFC.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI4:

    ?STEP :=   'TAIHI4  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｄ　４／８＃',TO-XCTL
    SAVFILE FILE-SETGKFD.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI5:

    ?STEP :=   'TAIHI5  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｅ　５／８＃',TO-XCTL
    SAVFILE FILE-SETGKFE.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI6:

    ?STEP :=   'TAIHI6  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｇ　６／８＃',TO-XCTL
    SAVFILE FILE-SETGKFG.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI7:

    ?STEP :=   'TAIHI7  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｈ　７／８＃',TO-XCTL
    SAVFILE FILE-SETGKFH.TOKFLIB,TODEV-LTO,REWIND-@NO,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

TAIHI8:

    ?STEP :=   'TAIHI8  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃請求ＤＴ退避Ｋ　８／８＃',TO-XCTL
    SAVFILE FILE-SETGKFK.TOKFLIB,TODEV-LTO,REWIND-@AFTER,
            MODE-@USED,COMPRESS-@YES
    IF   @PGMEC ^= 0 THEN
         GOTO  ABEND  END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃＃　請求締日データ退避終了　＃＃＃',TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
