# PSEDATBK

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSEDATBK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSEDATBK                             *  ./
/. *   JOB-NAME    :    退避処理（請求データ）多階層ＬＩＢ　 *  ./
/. *   UPDATE      :    2020/01/20 INOUE S11301570           *  ./
/. *                                     オフコンクラウド    *  ./
/. ***********************************************************  ./
    PGM  (P1-?PGCHK)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?PGCHK    ,STRING*1,IN,VALUE-' '        /.締チェック./

    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSEDATBK'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.社VAR       ?TAKAI1 ,STRING*15,VALUE-'/NAVPC/PSEDATBK' ./
    VAR       ?TAKAI1 ,STRING*15,VALUE-'/HGNAS/PSEDATBK'
    VAR       ?TAKAI2 ,STRING*99                  /.多階層LIB名./
    VAR       ?SIMEBI1,STRING*02,VALUE-'/1'       /.２０日締日./
    VAR       ?SIMEBI2,STRING*02,VALUE-'/2'       /.末日締日./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '請求締処理前バックアップ'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##バックアップ多階層ライブラリ名変換##./
    CASE  ?PGCHK OF
          # '1' #   ?TAKAI2 :=  ?TAKAI1 && ?SIMEBI1
                    ?MSGX := '##２０日締　バックアップ##'
                    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,
                           JLOG-@YES
                    ?OPR3 := '　（２０日締バックアップ）'
          # '2' #   ?TAKAI2 :=  ?TAKAI1 && ?SIMEBI2
                    ?MSGX := '##末日締　　バックアップ##'
                    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,
                           JLOG-@YES
                    ?OPR3 := '　（末日締　バックアップ）'
    ELSE
         ?MSGX := '##ﾊﾞｯｸｱｯﾌﾟ退避場所取得ｴﾗｰ##'
         SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,
                JLOG-@YES
              ?KEKA4 :=  '【ＢＫ場所取得エラー】'
              GOTO ABEND
    END

    ?MSGX :=  '##ﾊﾞｯｸｱｯﾌﾟ場所:' && ?TAKAI2 && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  請求締日処理前データ退避確認                                ./
STEP00:

    ?OPR1  :=  '　＃＃＃＃　前回請求締日データ退避処理　＃＃＃＃'
    ?OPR2  :=  '　前回分の請求締日データの退避処理を行ないます。'
    ?OPR4  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##退避処理##./
TAIHI1:

    ?STEP :=   'TAIHI1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SNDMSG MSG-'＃請求ＤＴ退避開始＃',TO-XCTL
    SAVFILE FILE-SETGKFA.TOKFLIB/SETGKFB.TOKFLIB/
            SETGKFC.TOKFLIB/SETGKFD.TOKFLIB/SETGKFE.TOKFLIB/
            SETGKFG.TOKFLIB/SETGKFH.TOKFLIB/SETGKFK.TOKFLIB/
            SETGK87.TOKKLIB/SETGK88.TOKKLIB/JHSEIKF.TOKDLIB/
            JHJSEKF.TOKDLIB,TODEV-@NONE,MODE-@USED,
            TOPATH-?TAKAI2,REP-@YES,COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF   ?PGMEC ^= 0 THEN
         ?KEKA4 :=  '【請求データバックアップ】'
         GOTO  ABEND  END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃＃　請求締日データ退避終了　＃＃＃',TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '請求締処理前バックアップに失敗しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
