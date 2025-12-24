# FUBAPING

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/FUBAPING.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧ部システム　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ネットワーク通信　　　　　           *  ./
/. *   JOB-ID      :    FUBAPING                            *  ./
/. *   JOB-NAME    :    ネットワーク通信                     *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'FUBAPING'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB
/.############./
/.##確　　認##./
/.############./


    ?OPR1  :=  '＃＃＃＃＃＃　フバサミ通信開始処理　＃＃＃＃＃＃'
    ?OPR2  :=  'フバサキ出荷検品データの送受信する為の処理を開始'
    ?OPR3  :=  'します。約５分程度、端末が使用できません。'
    ?OPR4  :=  '申し訳ありませんが、暫くお待ち下さい。'
    ?OPR5  :=  '※６月までの暫定処理です。６月以降はなくなります'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##通信開始1##./
FPING001:

    SNDMSG    MSG-'## PING   1/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始2##./
FPING002:

    SNDMSG    MSG-'## PING   2/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始3##./
FPING003:

    SNDMSG    MSG-'## PING   3/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始4##./
FPING004:

    SNDMSG    MSG-'## PING   4/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始5##./
FPING005:

    SNDMSG    MSG-'## PING   5/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始6##./
FPING006:

    SNDMSG    MSG-'## PING   6/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始7##./
FPING007:

    SNDMSG    MSG-'## PING   7/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始8##./
FPING008:

    SNDMSG    MSG-'## PING   8/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始9##./
FPING009:

    SNDMSG    MSG-'## PING   9/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始10##./
FPING010:

    SNDMSG    MSG-'## PING  10/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始11##./
FPING011:

    SNDMSG    MSG-'## PING  11/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始12##./
FPING012:

    SNDMSG    MSG-'## PING  12/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始13##./
FPING013:

    SNDMSG    MSG-'## PING  13/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始14##./
FPING014:

    SNDMSG    MSG-'## PING  14/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始15##./
FPING015:

    SNDMSG    MSG-'## PING  15/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始16##./
FPING016:

    SNDMSG    MSG-'## PING  16/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始17##./
FPING017:

    SNDMSG    MSG-'## PING  17/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始18##./
FPING018:

    SNDMSG    MSG-'## PING  18/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始19##./
FPING019:

    SNDMSG    MSG-'## PING  19/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始20##./
FPING020:

    SNDMSG    MSG-'## PING  20/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始21##./
FPING021:

    SNDMSG    MSG-'## PING  21/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始22##./
FPING022:

    SNDMSG    MSG-'## PING  22/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始23##./
FPING023:

    SNDMSG    MSG-'## PING  23/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始24##./
FPING024:

    SNDMSG    MSG-'## PING  24/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始25##./
FPING025:

    SNDMSG    MSG-'## PING  25/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始26##./
FPING026:

    SNDMSG    MSG-'## PING  26/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始27##./
FPING027:

    SNDMSG    MSG-'## PING  27/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始28##./
FPING028:

    SNDMSG    MSG-'## PING  28/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始29##./
FPING029:

    SNDMSG    MSG-'## PING  29/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始30##./
FPING030:

    SNDMSG    MSG-'## PING  30/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始31##./
FPING031:

    SNDMSG    MSG-'## PING  31/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始32##./
FPING032:

    SNDMSG    MSG-'## PING  32/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始33##./
FPING033:

    SNDMSG    MSG-'## PING  33/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始34##./
FPING034:

    SNDMSG    MSG-'## PING  34/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始35##./
FPING035:

    SNDMSG    MSG-'## PING  35/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始36##./
FPING036:

    SNDMSG    MSG-'## PING  36/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始37##./
FPING037:

    SNDMSG    MSG-'## PING  37/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始38##./
FPING038:

    SNDMSG    MSG-'## PING  38/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始39##./
FPING039:

    SNDMSG    MSG-'## PING  39/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始40##./
FPING040:

    SNDMSG    MSG-'## PING  40/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始41##./
FPING041:

    SNDMSG    MSG-'## PING  41/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始42##./
FPING042:

    SNDMSG    MSG-'## PING  42/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始43##./
FPING043:

    SNDMSG    MSG-'## PING  43/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始40##./
FPING044:

    SNDMSG    MSG-'## PING  44/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始45##./
FPING045:

    SNDMSG    MSG-'## PING  45/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始46##./
FPING046:

    SNDMSG    MSG-'## PING  46/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始47##./
FPING047:

    SNDMSG    MSG-'## PING  47/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始48##./
FPING048:

    SNDMSG    MSG-'## PING  48/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始49##./
FPING049:

    SNDMSG    MSG-'## PING  49/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')

/.##通信開始50##./
FPING050:

    SNDMSG    MSG-'## PING  50/50 ##',TO-XCTL

    CALL PING,PARA-('135.202.63.220')


RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

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
