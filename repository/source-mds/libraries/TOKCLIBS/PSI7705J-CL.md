# PSI7705J

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSI7705J.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    サンデー　　　支払照合               *  ./
/. *   JOB-ID      :    PSI7705J                             *  ./
/. *   JOB-NAME    :    照合データ削除                       *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI7705J'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?SYORINM1 :=  'サンデー殿　請求データ消込処理'

    ?OPR1  :=  '　＃＃＃　ジョイ　　　　請求ＤＴ消込処理　＃＃＃'
    ?OPR2  :=  '　請求データの消込処理を行います。　　　　　　　'
    ?OPR3  :=  '　宜しいですか？　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    DEFLIBL TOKELIB/TOKFLIB
/.##請求データ削除##./
SSI7705B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SITGKFS,TOFILE-SITGKFJ.TOKKLIB
    OVRF      FILE-SETGKFS2,TOFILE-SETGKFJ2.TOKKLIB
    CALL      PGM-SSI7705B.TOKELIBO,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'ジョイ　請求ＤＴ消込処理が正常終了しました。'
    ?SYORINM3 := '件数を確認して下さい。　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'ジョイ　請求ＤＴ消込処理が異常終了しました。'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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
