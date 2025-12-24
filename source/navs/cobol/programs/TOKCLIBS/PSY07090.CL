/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（ケーヨー新システム）    *  ./
/. *   SYSTEM-NAME :    配送発注集計表                       *  ./
/. *   JOB-ID      :    PSY07090                             *  ./
/. *   JOB-NAME    :    変換～コード変換～ルート取得～       *  ./
/. *               :    ＳＯＲＴ１～合計計算～ＳＯＲＴ２～   *  ./
/. *                    配送発注集計表                       *./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY07090'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '
    VAR       ?T        ,STRING*1,VALUE-'T'
    VAR       ?H        ,STRING*1,VALUE-'H'
    VAR       ?RB       ,STRING*2,VALUE-'RB'
    VAR       ?RM       ,STRING*2,VALUE-'RM'
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKWLIB '
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?RYOJNL1  ,NAME!MOD
    VAR       ?RYOWK    ,NAME!MOD
    VAR       ?HRYOJNL  ,NAME!MOD
    VAR       ?SHTBIKF1 ,NAME!MOD
    VAR       ?SHTBIKF2 ,NAME!MOD
    VAR       ?NHRYOJNL ,STRING*17,VALUE-'                 '
    VAR       ?NRYOJNL1 ,STRING*17,VALUE-'                 '
    VAR       ?NSHTBIKF ,STRING*17,VALUE-'                 '
    VAR       ?NSHTBIKL ,STRING*17,VALUE-'                 '
    VAR       ?WKRYO    ,STRING*17,VALUE-'                 '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾒﾓ番号取得##./
SSY0608B:

    ?STEP :=   'SSY0608B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY0608B.TOKELIB,PARA-(?WS,?MEMO)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 量販店ﾒﾓNO = ' && ?MEMO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
    ?FILNM    :=    ?T && ?MEMO && '1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOJNL1  :=    %NCAT(?FILID,?LIBID)
    ?NRYOJNL1 :=    %STRING(?RYOJNL1)
    ?MSGX     :=    '## 量販店JNL(LF1) = ' && ?NRYOJNL1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?T && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HRYOJNL  :=    %NCAT(?FILID,?LIBID)
    ?NHRYOJNL :=    %STRING(?HRYOJNL)
    ?MSGX     :=    '## 量販店JNL(PF)  = ' && ?NHRYOJNL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##量販店変換ワーク名取得##./
    ?FILNM    :=    ?H && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOWK    :=    %NCAT(?FILID,?LIBID)
    ?WKRYO    :=    %STRING(?RYOWK)
    ?MSGX     :=    '## 量販店ﾜｰｸF名   = ' && ?WKRYO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?RB && ?MEMO && '1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SHTBIKF2 :=    %NCAT(?FILID,?LIBID)
    ?NSHTBIKL :=    %STRING(?SHTBIKF2)
    ?MSGX     :=    '## 備考JNL(LF1)   = ' && ?NSHTBIKL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?RB && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SHTBIKF1 :=    %NCAT(?FILID,?LIBID)
    ?NSHTBIKF :=    %STRING(?SHTBIKF1)
    ?MSGX     :=    '## 備考JNK(PF)    = ' && ?NSHTBIKF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##処理確認##./
SSY0710I:

    ?STEP :=   'SSY0710I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0710I.TOKELIB,PARA-(?WS)
    IF        @PGMEC     =   4010 THEN
              ?MSGX := '##配送ルート表出力　取消終了##'
              SNDMSG MSG-?MSGX,TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販データＦコンバート##./
SSY0703B:

    ?STEP :=   'SSY0703B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HRUTJNL,TOFILE-?HRYOJNL
    OVRF      FILE-SHTDENWK,TOFILE-?RYOWK
    OVRF      FILE-SHTBIKL1,TOFILE-?SHTBIKF2
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY0703B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販店備考ﾜｰｸ初期化##./
    CLRFILE FILE-?SHTBIKF1

/.出荷場所，ルート，ルート順番をセット                          ./
SSY0709B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'***   ﾙｰﾄｼﾞｮｳﾎｳ ｾｯﾄ   ***',TO-XCTL


    OVRF      FILE-SHTDENLA,TOFILE-?RYOWK
    OVRF      FILE-SHWHISF,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENRMS1,TOFILE-TENRMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0709B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.ＳＯＲＴ(納品日，出荷場所，自社商品）                         ./
SORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHISF.TOKFLIB,INRL-256,INBF-1,
              OUTFILE-SHWHISF.TOKFLIB,OUTBF-1,
              KEY-54!5!CA,
              KEY1-38!2!CA,
              KEY2-74!16!CA,
              KEY3-199!13!CA,
              KEY4-157!5!CA,
              KEY5-162!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.商品毎数量集計                                                ./
SSY0018B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'***   ｼｮｳﾋﾝｺﾞｳｹｲ      ***',TO-XCTL

    OVRF      FILE-SHWHAIF,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-SHWHISGF,TOFILE-SHWHISGF.TOKFLIB
    CALL      PGM-SSY0711B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.合計ファイル                                            ./
CNVFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ワークファイル_配送伝票データへコピー',TO-XCTL

    CNVFILE   FILE-SHWHISGF.TOKFLIB,TOFILE-SHWHISF.TOKFLIB,
              ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CLRFILE   FILE-SHWHISGF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.ＳＯＲＴ（出場、納品、ルート、メモ、枝、自社商品、ルート順）./
SORT2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHISF.TOKFLIB,INRL-256,INBF-1,
              OUTFILE-SHWHISF.TOKFLIB,OUTBF-1,
              KEY-38!2!CA,
              KEY1-54!5!CA,
              KEY2-253!2!DA,
              KEY3-157!5!CA,
              KEY4-162!5!CA,
              KEY5-74!16!CA,
              KEY6-255!2!DA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.配送発注集計表出力                                            ./
SSY0019L:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'*** ﾊｲｿｳﾊｯﾁｭｳｼｭｳｹｲﾋｮｳ ***',TO-XCTL

    OVRF      FILE-INFILE,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TENRMS2,TOFILE-TENRMS2.TOKFLIB
    OVRF      FILE-HTOKMS,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0712L.TOKELIB
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
