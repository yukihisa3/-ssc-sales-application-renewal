# CVCSPC

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/CVCSPC.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    CVCSPC1                              *  ./
/. *   JOB-NAME    :    共通ＰＣ側受信処理                   *  ./
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
    VAR  ?TIMES  ,STRING*4,VALUE-'    '     /.受信開始./
    VAR  ?TIMEE  ,STRING*4,VALUE-'    '     /.受信終了./
/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'CVCSPC1 '  /.PROGRAM-ID./
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
/.##結果##./
    VAR  ?CHK1     ,STRING*1,VALUE-' '         /.ﾁｪｯｸ結果1./
    VAR  ?CHK2     ,STRING*1,VALUE-' '         /.ﾁｪｯｸ結果2./
    VAR  ?CHK3     ,STRING*1,VALUE-' '         /.ﾁｪｯｸ結果3./
    VAR  ?CHK4     ,STRING*1,VALUE-' '         /.ﾁｪｯｸ結果3./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR  ?FILNMF   ,STRING*8,VALUE-'        '    /.ﾍﾝｼｭｳﾌｧｲﾙ名格納./
    VAR  ?FILNMB   ,STRING*3,VALUE-'BAK'         /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR  ?LIBNMB   ,STRING*8,VALUE-'ONLBLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR  ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR  ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR  ?ONLFI1F  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR  ?ONLFI1FN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR  ?ONLFI2F  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR  ?ONLFI2FN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR  ?HENTOK   ,STRING*5,VALUE-'     '       /.ﾍﾝｶﾝﾄｸｲｻｷCD./

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　　自動受信開始　　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　　（ＰＣ－１）　　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　　（待ち受け）　　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES

   /.##ｼﾞｭｼﾝｶｲｼ TIME ｼｭﾄｸ##./
    ?JTIMES  := @STIME
    ?JTIMESH := %SBSTR(?JTIMES,1,4)

/.##パラメタファイルより受信パラメタ取得##./
SCV0150B:

    ?STEP :=   'SCV0150B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0150B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?KEKA,?TIMES,?TIMEE,?JRL,?TOKNM)
    IF  @PGMEC ^= 0 THEN
        ?KEKA  :=  '70'
        GOTO  ABEND2
    END

   /.##結果更新##./
    DEFLIBL TOKELIB/TOKFLIB
    ?KEKA := '64'
    CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:
    /.##　受信元ﾌｧｲﾙ　##./
    ?FILNMF   :=    ?FILNM                /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNMF)        /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNMB)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?ONLFI1F  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?ONLFI1FN :=    %STRING(?ONLFI1F)
    ?MSGX     :=    '## 受信ﾌｧｲﾙ名 = ' && ?ONLFI1FN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /.##　バックアップＦ　##./
    ?HENTOK   :=    %SBSTR(?TOKCD,4,5)    /.ﾄﾘﾋｷｻｷCDﾍﾝｼｭｳ./
    ?FILNMF   :=    ?FILNMB && ?HENTOK    /.ﾊﾞｯｸｱｯﾌﾟﾌｧｲﾙ名+ﾄﾘﾋｷｻｷ./
    ?FILID    :=    %NAME(?FILNMF)        /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNMB)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?ONLFI2F  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?ONLFI2FN :=    %STRING(?ONLFI2F)
    ?MSGX     :=    '## 受信BACKUP = ' && ?ONLFI2FN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##待ち受け処理##./
TWAIT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    MSG-'##ＰＣ全銀受信待機中##',TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    CALL      PGM-WAITPG.TOKELIB,PARA-('000030')
    IF        @PGMEC  ^=  0  THEN
              ?KEKA  :=  '52'
              GOTO ABEND1
    END

/.##受信確認Ｆチェック##./
SPC0010B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-PCCHK1,TOFILE-PCCHK1.TOKFLIB
    CALL      PGM-SPC0010B.TOKELIB,PARA-(?CHK1,?TOKCD)
    ?MSGX :=  '?CHK = ' && ?CHK1
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    IF        @PGMEC  ^=  0  THEN
              GOTO TWAIT
    ELSE
              SNDMSG ?CHK1,TO-XCTL.@ORGPROF
              IF   ?CHK1 = '0'  THEN
                   GOTO  TWAIT
              END
              IF   ?CHK1 = '2'  THEN
                   ?KEKA  :=  '61'
                   GOTO  ABEND1
              END
              IF   ?CHK1 = '3'  THEN
                   ?KEKA  :=  '65'
                   GOTO  ABEND1
              END
    END

/.##受信終了時間取得##./
ENDTIME:
   /.##ｼﾞｭｼﾝｼｭｳﾘｮｳTIME ｼｭﾄｸ##./
    ?JTIMEE  := @STIME
    ?JTIMEEH := %SBSTR(?JTIMEE,1,4)

/.##受信ＤＴ存在チェック##./
SPC0020B:

    IF  ?JRL  =  '2048'  THEN
         ?STEP :=   'SPC0020B'
         ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

         OVRF FILE-NPS,TOFILE-?ONLFI1F
         CALL PGM-SPC0020B.TOKELIB,PARA-(?CHK2)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '53'
             GOTO  ABEND2
         ELSE
             IF   ?CHK2 = '0'  THEN
                  ?KEKA  :=  '62'
                  GOTO  ABEND1
             END
         END
    END

    IF  ?JRL  =  '0256'  THEN
         ?STEP :=   'SPC0040B'
         ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

         OVRF FILE-NPS,TOFILE-?ONLFI1F
         CALL PGM-SPC0040B.TOKELIB,PARA-(?CHK2)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '53'
             GOTO  ABEND2
         ELSE
             IF   ?CHK2 = '0'  THEN
                  ?KEKA  :=  '62'
                  GOTO  ABEND1
             END
         END
    END

    IF  ?JRL  =  '0128'  THEN
         ?STEP :=   'SPC0050B'
         ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

         OVRF FILE-NPS,TOFILE-?ONLFI1F
         CALL PGM-SPC0050B.TOKELIB,PARA-(?CHK4)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '53'
             GOTO  ABEND2
         ELSE
             IF   ?CHK4 = '0'  THEN
                  ?KEKA  :=  '62'
                  GOTO  ABEND1
             END
         END
    END

    IF  ?JRL  =  '0264'  THEN
         ?STEP :=   'SPC0060B'
         ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

         OVRF FILE-NPS,TOFILE-?ONLFI1F
         CALL PGM-SPC0060B.TOKELIB,PARA-(?CHK4)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '53'
             GOTO  ABEND2
         ELSE
             IF   ?CHK4 = '0'  THEN
                  ?KEKA  :=  '62'
                  GOTO  ABEND1
             END
         END
    END

/.##受信開始時間／終了時間パラメタファイルセット##./
TIMESET:

    ?STEP :=   'TIMESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0140B.TOKELIB,PARA-(?JTIMESH,?JTIMEEH)
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        ?KEKA := '54'
        GOTO   ABEND1
    END

/.##受信データ変換処理##./
HENPC1:

    ?STEP :=   'HENPC1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL PGM-HENPC1.TOKELIB,PARA-(?CHK3)
    IF   ?CHK3 = '1'  THEN
         ?KEKA  :=  '63'
         GOTO  ABEND2
    END

/.##受信ファイルＧＰ６０００へ転送##./
BACKUP:

    ?STEP :=   'BACKUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CNVFILE FILE-?ONLFI1F,TOFILE-?ONLFI2F,ADD-@NO,BF-1
    IF  @PGMEC ^= 0 THEN
        ?KEKA := '55'
        GOTO   ABEND1
    END

/.##受信Ｆ／受信確認Ｆクリア##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE FILE-?ONLFI1F
    IF  @PGMEC ^= 0 THEN
        ?KEKA := '56'
        GOTO   ABEND2
    END

    CLRFILE FILE-PCCHK1.TOKFLIB
    IF  @PGMEC ^= 0 THEN
        ?KEKA := '57'
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
    DEFLIBL TOKELIB/TOKFLIB
    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0130B.TOKELIB,PARA-(?KEKA)
   /.##エラー処理#./
    CALL PGM-ERRPC1.TOKELIB
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
/.  DSPLOG OUTPUT-@LIST   ./
    RETURN    PGMEC-@PGMEC

ABEND2:
   /.##結果区分更新##./
    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0130B.TOKELIB,PARA-(?KEKA)
   /.##エラー処理#./
    CALL PGM-ERRPC1.TOKELIB
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
    SNDMSG MSG-'＃　＜受信処理変換中にエラーが発生＞　＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

```
