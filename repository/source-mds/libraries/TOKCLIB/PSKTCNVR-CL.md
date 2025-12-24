# PSKTCNVR

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSKTCNVR.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オンライン                           *  ./
/. *   JOB-ID      :    PSKTCNVR                             *  ./
/. *   JOB-NAME    :    量販店データ変換（ルート）          *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSKTCNVR'
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
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?RYOJNL1  ,NAME!MOD
    VAR       ?RYOWK    ,NAME!MOD
    VAR       ?HRYOJNL  ,NAME!MOD
    VAR       ?SHTBIKF1 ,NAME!MOD
    VAR       ?SHTBIKF2 ,NAME!MOD
    VAR       ?DENMEISF ,NAME!MOD
    VAR       ?NHRYOJNL ,STRING*17,VALUE-'                 '
    VAR       ?NRYOJNL1 ,STRING*17,VALUE-'                 '
    VAR       ?NSHTBIKF ,STRING*17,VALUE-'                 '
    VAR       ?NSHTBIKL ,STRING*17,VALUE-'                 '
    VAR       ?WKRYO    ,STRING*17,VALUE-'                 '
    VAR       ?DENMEI   ,STRING*17,VALUE-'                 '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾒﾓ番号取得##./
SSY0708B:

    ?STEP :=   'SSY0708B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY0708B.TOKELIB,PARA-(?WS,?MEMO)
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

    ?FILNM    :=    ?H && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOWK    :=    %NCAT(?FILID,?LIBID)
    ?WKRYO    :=    %STRING(?RYOWK)
    ?MSGX     :=    '## 量販店ﾜｰｸF名   = ' && ?WKRYO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?RM && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DENMEISF :=    %NCAT(?FILID,?LIBID)
    ?DENMEI   :=    %STRING(?DENMEISF)
    ?MSGX     :=    '## 伝票明細WK名   = ' && ?DENMEI
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##処理確認##./
SSY0707I:

    ?STEP :=   'SSY0707I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0707I.TOKELIB
    IF        @PGMEC     =   155  THEN
              SNDMSG MSG-'***   取消終了 OSKT045   ***',TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販店備考ﾜｰｸ初期化##./
    CLRFILE FILE-?SHTBIKF1
/.##量販データＦコンバート##./
SSY0703B:

    ?STEP :=   'SSY0703B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-HRUTJNL,TOFILE-?HRYOJNL
    OVRF      FILE-SHTDENWK,TOFILE-?RYOWK
    OVRF      FILE-SHTBIKL1,TOFILE-?SHTBIKF2
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY0703B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##SORT1##./
SORT1:

    ?STEP :=   'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.取引先、納品日、出荷場所、伝票番号、店舗コード、行番号./
     SORT     INFILE-?RYOWK,INRL-1020,INBF-1,
              OUTFILE-?RYOWK,OUTBF-1,
              KEY-1!8!CA,KEY1-54!5!PA,KEY2-38!2!CA,KEY3-9!9!CA,
              KEY4-33!5!CA,KEY5-18!2!CA,
              SEQ-@YES,RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##伝票_ＣＮＴ##./
SSY0704B:

    ?STEP :=   'SSY0704B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-KYOTUX,TOFILE-?RYOWK
    OVRF      FILE-SHTBIKL1,TOFILE-?SHTBIKF2
    OVRF      FILE-DENMEISF,TOFILE-?DENMEISF
    OVRF      FILE-HDENJNL,TOFILE-SHTDENF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    CALL      PGM-SSY0704B.TOKELIB,PARA-(?WS)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##条件Ｆメモ_更新##./
SSY0706B:

    ?STEP :=   'SSY0706B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY0706B.TOKELIB,PARA-(?WS)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.##端末別量販店ジャーナル初期化##./
    CLRFILE FILE-?HRYOJNL

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
