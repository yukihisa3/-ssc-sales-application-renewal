/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    日次業務                             *  ./
/. *   JOB-ID      :    ACOSPG1                              *  ./
/. *   JOB-NAME    :    本社／各営業所　計上ＤＴ作成         *  ./
/. ***********************************************************  ./
    PGM
/.##ﾜｰｸｱﾘｱ##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ACOSPG1 '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB


/.################################################################./
/.# 件数リスト発行                                               #./
/.################################################################./
STEP2:

    CALL SCVMSG1.TOKELIB,PARA-('STEP3=>ｹﾝｽｳﾘｽﾄ ﾊｯｺｳ ')

    OVRF      FILE-HOU,TOFILE-TOKUWK.TOKWLIB
    OVRF      FILE-HOS,TOFILE-TOKUZ.TOKFLIB
    OVRF      FILE-FKU,TOFILE-TOKUFKU.TOKFLIB
    OVRF      FILE-FKS,TOFILE-TOKUFKS.TOKFLIB
    OVRF      FILE-SEU,TOFILE-TOKUSEU.TOKFLIB
    OVRF      FILE-SES,TOFILE-TOKUSES.TOKFLIB
    OVRF      FILE-HKU,TOFILE-TOKUHKU.TOKFLIB
    OVRF      FILE-HKS,TOFILE-TOKUHKS.TOKFLIB
    OVRF      FILE-OSU,TOFILE-TOKUOSU.TOKFLIB
    OVRF      FILE-OSS,TOFILE-TOKUOSS.TOKFLIB
    OVRF      FILE-OKU,TOFILE-TOKUOKU.TOKFLIB
    OVRF      FILE-OKS,TOFILE-TOKUOKS.TOKFLIB
    CALL      PGM-SKY2301B.TOKELIB
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END


RTN:

    DEFLIBL TOKELIB/TOKFLIB
    CALL SKY3401B.TOKELIB,PARA-('1')
    CALL SCVMSG1.TOKELIB,PARA-('ACOSPG1 ｾｲｼﾞｮｳ END  ')
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB
    CALL SKY3401B.TOKELIB,PARA-('2')
    CALL SCVMSG1.TOKELIB,PARA-('ACOSPG1 ｲｼﾞｮｳ  END  ')
    RETURN    PGMEC-@PGMEC
