# CSKTCNV

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/CSKTCNV.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オンライン                           *  ./
/. *   JOB-ID      :    CSKTCNV                              *  ./
/. *   JOB-NAME    :    量販店データ変換（複数端末変換可能） *  ./
/. *   2008/08/08 内部統制対応                               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CSKTCNV '
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '
    VAR       ?R        ,STRING*1,VALUE-'R'
    VAR       ?W        ,STRING*1,VALUE-'W'
    VAR       ?B        ,STRING*1,VALUE-'B'
    VAR       ?M        ,STRING*1,VALUE-'M'
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKWLIB '
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
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKSOLIB/TOKMDLIB/TOKSOLIB
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

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
    ?FILNM    :=    ?R && ?MEMO && '1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOJNL1  :=    %NCAT(?FILID,?LIBID)
    ?NRYOJNL1 :=    %STRING(?RYOJNL1)
    ?MSGX     :=    '## 量販店JNL(LF1) = ' && ?NRYOJNL1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?R && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HRYOJNL  :=    %NCAT(?FILID,?LIBID)
    ?NHRYOJNL :=    %STRING(?HRYOJNL)
    ?MSGX     :=    '## 量販店JNL(PF)  = ' && ?NHRYOJNL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?B && ?MEMO && '1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SHTBIKF2 :=    %NCAT(?FILID,?LIBID)
    ?NSHTBIKL :=    %STRING(?SHTBIKF2)
    ?MSGX     :=    '## 備考JNL(LF1)   = ' && ?NSHTBIKL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?B && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SHTBIKF1 :=    %NCAT(?FILID,?LIBID)
    ?NSHTBIKF :=    %STRING(?SHTBIKF1)
    ?MSGX     :=    '## 備考JNL(PF)    = ' && ?NSHTBIKF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?W && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOWK    :=    %NCAT(?FILID,?LIBID)
    ?WKRYO    :=    %STRING(?RYOWK)
    ?MSGX     :=    '## 量販店ﾜｰｸF名   = ' && ?WKRYO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?M && ?MEMO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DENMEISF :=    %NCAT(?FILID,?LIBID)
    ?DENMEI   :=    %STRING(?DENMEISF)
    ?MSGX     :=    '## 伝票明細WK名   = ' && ?DENMEI
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##担当者CD指定##./
/.##更新担当者を入力させる（追加）データ担当者は全て更新担当者に##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO  ABEND
              END
    ELSE
              ?MSGX :=  '##担当者 = ' && ?TANCD && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '##部門CD = ' && ?BUMON && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.##処理確認##./
SSY0607I:

    ?STEP :=   'SSY0607I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0607I.TOKELIB
    IF        @PGMEC     =   155  THEN
              SNDMSG MSG-'***   取消終了 OSKT045   ***',TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販店備考ﾜｰｸ初期化##./
    CLRFILE FILE-?SHTBIKF1
/.##量販データＦコンバート##./
SSY0603B:

    ?STEP :=   'SSY0603B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTRYTF,TOFILE-?HRYOJNL
    OVRF      FILE-SHTDENWK,TOFILE-?RYOWK
    OVRF      FILE-SHTBIKL1,TOFILE-?SHTBIKF2
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY0603B.TOKELIB
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
TSY0604B:

    ?STEP :=   'TSY0604B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KYOTUX,TOFILE-?RYOWK
    OVRF      FILE-HDENJNL,TOFILE-SHTDENF.TOKFLIB
    OVRF      FILE-SHTBIKL1,TOFILE-?SHTBIKF2
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-DENMEISF,TOFILE-?DENMEISF
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-TSY0604B.TOKELIBO,PARA-(?WS,?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##条件Ｆメモ_更新##./
SSY0606B:

    ?STEP :=   'SSY0606B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY0606B.TOKELIB,PARA-(?WS)
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
