# PLST060

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PLST060.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタリスト                         *  ./
/. *   JOB-ID      :    PLST060                              *  ./
/. *   JOB-NAME    :    担当者マスタ保守                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PLST060 '
    VAR       ?STEP     ,STRING*8
    VAR       ?SOUSASYU ,STRING*1,VALUE-' '        /.部門名./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.担当者CD./
    VAR       ?TANST    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?TANED    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?NYUST    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?NYUED    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?PMODE    ,STRING*1,VALUE-' '        /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.帳票出力ﾊﾟﾗﾒﾀ./
    VAR       ?STIME    ,STRING*4,VALUE-'0000'     /.時刻./
    VAR       ?ETIME    ,STRING*4,VALUE-'9999'     /.時刻./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

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

/.##画面範囲指定##./
SLST060I:

    ?STEP :=   'SLST060I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-HSHOTBR3,TOFILE-HSHOTBR3.TOKKLIB
    CALL      PGM-SLST060I.TOKELIBO,PARA-(?SOUSASYU,?TANST,?TANED,
                                          ?NYUST,?NYUED,?PMODE)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO  ABEND
              END
    ELSE
              ?MSGX :=  '操作種別 = ' && ?SOUSASYU
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '担当開始 = ' && ?TANST
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '担当終了 = ' && ?TANED
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '日付開始 = ' && ?NYUST
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '日付終了 = ' && ?NYUED
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  'モード　 = ' && ?PMODE
              SNDMSG    ?MSGX,TO-XCTL
              ?P1 := ?SOUSASYU && ?BUMON && ?TANST && ?TANED &&
                     ?NYUST && ?NYUED && ?PMODE && ?STIME && ?ETIME
              ?MSGX :=  'パラメタ = ' && ?P1
              SNDMSG    ?MSGX,TO-XCTL
    END

/.##商品変換テーブルリスト##./
SLST060:

    ?STEP :=   'SLST060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?BUMON = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-HSHOTBR2,TOFILE-HSHOTBR2.TOKKLIB
    OVRF     FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
/.  CALL PGM-SLST060.TOKELIBO,PARA-(?P1)
./  CALL PGM-SLST065.TOKELIBO,PARA-(?P1)
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

```
