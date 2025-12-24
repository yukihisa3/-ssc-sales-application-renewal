# CVCSINS1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/CVCSINS1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    CVCSINS1                             *  ./
/. *   JOB-NAME    :    共通Ｋ６５００側受信処理             *  ./
/. ***********************************************************  ./
    PGM
/.##パラメタ定義##./
    VAR  ?HIDUKE ,STRING*8,VALUE-'        ' /.受信日付./
    VAR  ?JIKAN  ,STRING*4,VALUE-'    '     /.受信時間./
    VAR  ?TOKCD  ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR  ?PASS   ,STRING*13,VALUE-'             '
    VAR  ?HENID  ,STRING*8,VALUE-'        ' /.変換ID./
    VAR  ?LINE   ,STRING*1,VALUE-' '        /.回線./
    VAR  ?YUSEN  ,STRING*1,VALUE-' '        /.回線優先./
    VAR  ?LIBNM  ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR  ?FILNM  ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR  ?KJOB   ,STRING*8,VALUE-'        ' /.起動JOB./
    VAR  ?JRL    ,STRING*4,VALUE-'    '     /.ﾚｺｰﾄﾞ長./
    VAR  ?TOKNM  ,STRING*15,VALUE-'               ' /.ﾄﾘﾋｷｻｷﾒｲｼｮｳ./
/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'CVCSINS1'  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
/.##結果区分定義##./
    VAR  ?KEKA     ,STRING*2,VALUE-'00'        /.更新ｹｯｶFLG./
/.##リダイヤルカウント##./
    VAR  ?RETEL    ,INTEGER                    /.ﾘﾀﾞｲﾔﾙ./
/.##受信開始／終了時間##./
    VAR  ?JTIMES   ,STRING*6,VALUE-'      '    /.ｶｲｼｼﾞｶﾝ./
    VAR  ?JTIMESH  ,STRING*4,VALUE-'    '      /.ﾀｲﾋｶｲｼｼﾞｶﾝ./
    VAR  ?JTIMEE   ,STRING*6,VALUE-'      '    /.ｼｭｳﾘｮｳｼﾞｶﾝ./
    VAR  ?JTIMEEH  ,STRING*4,VALUE-'    '      /.ﾀｲﾋｼｭｳﾘｮｳｼﾞｶﾝ./
/.##数値型へ変更##./
    VAR  ?SIZE     ,INTEGER                    /.ﾚｺｰﾄﾞ長(数値)./

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　　自動受信開始　　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　（ＩＳＤＮ－１）　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES

/.##パラメタファイルより受信パラメタ取得##./
SCV0110B:

    ?STEP :=   'SCV0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0110B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?TOKNM)
    IF  @PGMEC ^= 0 THEN
        ?KEKA  :=  '83'
        GOTO  ABEND1
    END
/.##受信ﾚｺｰﾄﾞ長　数値変換##./
    ?SIZE := %INTEGER(?JRL)
/.##ＣＶＣＳ受信処理開始##./
CVCS1001:

    ?STEP :=   'CVCS1001'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?MSGX :=  '### 受信開始'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '### 取引先 = ' && ?TOKCD && ':' && ?TOKNM
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /.##ﾘﾀﾞｲﾔﾙFLGｾｯﾄ##./
    ?RETEL := 1
/.##リダイヤル位置##./
RETEL:
   /.##ｼﾞｭｼﾝｶｲｼ TIME ｼｭﾄｸ##./
    ?JTIMES  := @STIME
    ?JTIMESH := %SBSTR(?JTIMES,1,4)
   /.##ｼﾞｭｼﾝｶﾝｹｲﾌｧｲﾙ ｵｷｶｴ##./
/.  OVRVLDF  FILE-LD901,TOFILE-LD901.CVCSLBI
    OVRVTMF  FILE-DICVCS01,AFILE-DOCVCS01,TOFILE-DICVCS01.CVCSLBI
    OVRISF   FILE-CVCSI001,TOFILE-CVCSI001.CVCSLBI
    OVRF     FILE-CVCSG001,TOFILE-CVCSG001.CVCSLBI        ./
   /.##ｼﾞｭｼﾝﾌﾟﾛｸﾞﾗﾑ START##./
/.  CALL     PGM-CVCS1001.CVCSOBJ,PARA-(?PASS)            ./
    ?PGMEC := @PGMEC
    IF ?PGMEC ^= 0  THEN
       IF ?PGMEC = 4095 THEN
          IF ?RETEL < 5  THEN
             ?MSGX := '### ﾘﾀﾞｲﾔﾙ ﾀｲｷ ﾁｭｳ ###'
             SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
             ?RETEL := ?RETEL + 1
             /.### ｼﾞｭｼﾝﾏﾁｱﾜｾ ###./
             CALL TWAIT.XUCL
             GOTO   RETEL
           ELSE
             ?KEKA := '84'
             GOTO   ABEND1
           END
       ELSE
           ?KEKA := '85'
           GOTO   ABEND1
       END
    END
/.##受信終了時間取得##./
ENDTIME:
   /.##ｼﾞｭｼﾝｼｭｳﾘｮｳTIME ｼｭﾄｸ##./
    ?JTIMEE  := @STIME
    ?JTIMEEH := %SBSTR(?JTIMES,1,4)
/.##受信開始時間／終了時間パラメタファイルセット##./
TIMESET:

    ?STEP :=   'TIMESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0140B.TOKELIB,PARA-(?JTIMESH,?JTIMEEH)
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        ?KEKA := '86'
        GOTO   ABEND1
    END

/.##受信ファイルＧＰ６０００へ転送##./
SNDFILE:

    ?STEP :=   'SNDFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /. SNDFILE COM-PCPATH02.PCNODE02,SFILE-CVCSG001.CVCSLBI,
            DFILE-?FILNM,RL-?SIZE,BF-1  ./
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        ?KEKA := '87'
        GOTO   ABEND1
    END

/.##パラメタファイルＧＰ６０００へ転送##./
PARASND:

    ?STEP :=   'PARASND '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  SNDFILE COM-PCPATH02.PCNODE02,SFILE-PARAINS1.TOKFLIB,
            DFILE-'PARAINS1',RL-150,BF-1  ./
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        ?MSGX := '## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ ERR ##'
        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        ?KEKA := '88'
        GOTO   ABEND2
    END

/.##ＧＰ６０００側　ＪＯＢ起動##./
SNDRSJJ:

    ?STEP :=   'SNDRSJJ '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /. SNDRSJJ COM-PCPATH02.PCNODE02,DFILE-'HENINS1'  ./
    CALL PGM-HENINS1.TOKELIB
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        ?MSGX := '## GP6000 ｶﾞﾜ JOB ｷﾄﾞｳ ERR ##'
        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        ?KEKA := '89'
        GOTO   ABEND2
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND1:
   /.##結果区分更新##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0130B.TOKELIB,PARA-(?KEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES
    DSPLOG OUTPUT-@LIST
    GOTO    PARASND     /.## ﾊﾟﾗﾒﾀﾌｧｲﾙ 送信 ﾉ ﾀﾒ GOTO ##./

ABEND2:
   /.##結果区分更新##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0130B.TOKELIB,PARA-(?KEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES
    DSPLOG OUTPUT-@LIST
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃＜ＧＰ６０００送信時にエラーが発生＞＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

```
