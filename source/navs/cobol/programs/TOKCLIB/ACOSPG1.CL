/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    日次業務                             *  ./
/. *   JOB-ID      :    ACOSPG1                              *  ./
/. *   JOB-NAME    :    本社／各営業所　計上ＤＴ作成         *  ./
/. *   UPDATE      :    20120301　基幹サーバ統合             *  ./
/. *                    実績用計上ワーククリア追加           *  ./
/. *   UPDATE      :    20120327　MO->LTO                    *  ./
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

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    CALL SCVMSG1.TOKELIB,PARA-('ACOSPG1 START       ')
/.################################################################./
/.# 本社／各営業所データバックアップ                             #./
/.################################################################./
STEP1:

    CALL SCVMSG1.TOKELIB,PARA-('STEP1 ==> BACKUP    ')
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｳﾘｱｹﾞF BKUP    ')
    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-TOKUHOU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｳﾘｱｹﾞF BKUP-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｼｲﾚF BKUP      ')
    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-TOKUHOS.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｼｲﾚF BKUP-NG   ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｳﾘｱｹﾞF BKUP    ')
    CNVFILE FILE-TOKUOSU.TOKFLIB,TOFILE-TOKUOSU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｳﾘｱｹﾞF BKUP-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｼｲﾚF BKUP      ')
    CNVFILE FILE-TOKUOSS.TOKFLIB,TOFILE-TOKUOSS.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｼｲﾚF BKUP-NG   ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｳﾘｱｹﾞF BKUP    ')
    CNVFILE FILE-TOKUFKU.TOKFLIB,TOFILE-TOKUFKU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｳﾘｱｹﾞF BKUP-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｼｲﾚF BKUP      ')
    CNVFILE FILE-TOKUFKS.TOKFLIB,TOFILE-TOKUFKS.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｼｲﾚF BKUP-NG   ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｳﾘｱｹﾞF BAKUP  ')
    CNVFILE FILE-TOKUSEU.TOKFLIB,TOFILE-TOKUSEU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｳﾘｱｹﾞF BKUP-NG')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｼｲﾚF BKUP     ')
    CNVFILE FILE-TOKUSES.TOKFLIB,TOFILE-TOKUSES.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｼｲﾚF BAKUP-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｳﾘｱｹﾞF BKUP ')
    CNVFILE FILE-TOKUHKU.TOKFLIB,TOFILE-TOKUHKU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｳﾘｱｹﾞF BK-NG')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｼｲﾚF BAKUP  ')
    CNVFILE FILE-TOKUHKS.TOKFLIB,TOFILE-TOKUHKS.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｼｲﾚF BAUP-NG')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｳﾘｱｹﾞF BKUP ')
    CNVFILE FILE-TOKUOKU.TOKFLIB,TOFILE-TOKUOKU.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｳﾘｱｹﾞF BK-NG')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｼｲﾚF BAKUP  ')
    CNVFILE FILE-TOKUOKS.TOKFLIB,TOFILE-TOKUOKS.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｼｲﾚF BAUP-NG')
            GOTO ABEND
    END

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

/.################################################################./
/.# 売上データ計上Ｆへコピー（コマンド使用）                     #./
/.################################################################./
STEP3:

    CALL SCVMSG1.TOKELIB,PARA-('STEP3=> FILE->ACOSWK')

/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｳﾘｱｹﾞF COPY    ')
    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｳﾘｱｹﾞF COPY-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｳﾘｱｹﾞF COPY    ')
    CNVFILE FILE-TOKUOSU.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｳﾘｱｹﾞF COPY-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｳﾘｱｹﾞF COPY    ')
    CNVFILE FILE-TOKUFKU.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｳﾘｱｹﾞF COPY-NG  ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｳﾘｱｹﾞF COPY   ')
    CNVFILE FILE-TOKUSEU.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｳﾘｱｹﾞF COPY-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｳﾘｱｹﾞF COPY ')
    CNVFILE FILE-TOKUHKU.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｳﾘｱｹﾞF CP-NG ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｳﾘｱｹﾞF COPY ')
    CNVFILE FILE-TOKUOKU.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｳﾘｱｹﾞF CP-NG ')
            GOTO ABEND
    END

/.################################################################./
/.# 仕入データ計上Ｆへコピー（コマンド使用）                     #./
/.################################################################./
STEP4:

    CALL SCVMSG1.TOKELIB,PARA-('STEP4=> FILE->ACOSWK')

/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｼｲﾚF COPY      ')
    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎﾝｼｬ ｼｲﾚF COPY-NG    ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｼｲﾚF COPY      ')
    CNVFILE FILE-TOKUOSS.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｵｻｶ ｼｲﾚF COPY-NG    ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｼｲﾚF COPY      ')
    CNVFILE FILE-TOKUFKS.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾌｸｵｶ ｼｲﾚF COPY-NG    ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｼｲﾚF COPY     ')
    CNVFILE FILE-TOKUSES.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｾﾝﾀﾞｲ ｼｲﾚF COPY-NG  ')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｼｲﾚF COPY   ')
    CNVFILE FILE-TOKUHKS.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ﾎｯｶｲﾄﾞｳ ｼｲﾚF COPY-NG')
            GOTO ABEND
    END
/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｼｲﾚF COPY   ')
    CNVFILE FILE-TOKUOKS.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ｵｶﾔﾏ    ｼｲﾚF COPY-NG')
            GOTO ABEND
    END

/.################################################################./
/.# ＡＣＯＳ計上データ　バックアップ                             #./
/.################################################################./
STEP5:

    CALL SCVMSG1.TOKELIB,PARA-('STEP5=> ACOSWK->BLIB')

/.################################################################./
    CALL SCVMSG1.TOKELIB,PARA-('ACOSWK -> BLIB COPY ')
    CNVFILE FILE-ACOSWK.TOKWLIB,TOFILE-ACOSWK.TOKBLIB,ADD-@NO,
            BF-1
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('ACOSWK -> BLIB CP-NG')
            GOTO ABEND
    END

/.################################################################./
/.# 本社／各営業所　計上データ初期化                             #./
/.################################################################./
STEP6:

    CALL SCVMSG1.TOKELIB,PARA-('STEP6=> CLRFILE     ')


    CALL SCVMSG1.TOKELIB,PARA-('TOKUWK CLRFILE      ')
    CLRFILE FILE-TOKUWK.TOKWLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUWK CLRFILE-NG   ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUZ  CLRFILE      ')
    CLRFILE FILE-TOKUZ.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUZ  CLRFILE-NG   ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUOSU CLRFILE     ')
    CLRFILE FILE-TOKUOSU.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUOSU CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUOSS CLRFILE     ')
    CLRFILE FILE-TOKUOSS.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUOSS CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUFKU CLRFILE     ')
    CLRFILE FILE-TOKUFKU.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUFKU CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUFKS CLRFILE     ')
    CLRFILE FILE-TOKUFKS.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUFKS CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUSEU CLRFILE     ')
    CLRFILE FILE-TOKUSEU.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUSEU CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUSES CLRFILE     ')
    CLRFILE FILE-TOKUSES.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUSES CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUHKU CLRFILE     ')
    CLRFILE FILE-TOKUHKU.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUHKU CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUHKS CLRFILE     ')
    CLRFILE FILE-TOKUHKS.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUHKS CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUOKU CLRFILE     ')
    CLRFILE FILE-TOKUOKU.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUOKU CLRFILE-NG  ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUOKS CLRFILE     ')
    CLRFILE FILE-TOKUOKS.TOKFLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUOKS CLRFILE-NG  ')
            GOTO ABEND
    END
    CALL SCVMSG1.TOKELIB,PARA-('TOKUJWK CLRFILE      ')
    CLRFILE FILE-TOKUJWK.TOKWLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUJWK CLRFILE-NG   ')
            GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('TOKUJZ  CLRFILE      ')
    CLRFILE FILE-TOKUJZ.TOKKLIB
    IF      @PGMEC ^= 0 THEN
            CALL SCVMSG1.TOKELIB,PARA-('TOKUJZ CLRFILE-NG   ')
            GOTO ABEND
    END

/.################################################################./
/.# ＡＣＯＳ計上Ｆ　ＳＯＲＴ                                     #./
/.################################################################./
STEP7:

    CALL SCVMSG1.TOKELIB,PARA-('STEP7=> SORT        ')

/.  SORT    INFILE-ACOSWK.TOKWLIB,INRL-256,INBF-1,
            OUTFILE-ACOSWK.TOKWLIB,OUTBF-1,
            KEY-1!26!CA,
            RCDL-@DSP
./
/.## 2012/04/23 NAV TAKAHASHI ST　ソート順を変更　　　　　　　　##./
/.## 部門、伝区、取ＣＤ、伝票番号、相殺、店ＣＤ、納品日、行番号 ##./
    SORT    INFILE-ACOSWK.TOKWLIB,INRL-256,INBF-1,
            OUTFILE-ACOSWK.TOKWLIB,OUTBF-1,
            WORKFILE-ACOSSORT.TOKWLIB,
            KEY-1!4!CA,
            KEY1-5!2!CA,
            KEY2-19!8!CA,
            KEY3-7!9!CA,
            KEY4-18!1!CA,
            KEY5-27!5!CA,
            KEY6-40!8!CA,
            KEY7-16!2!CA,
            RCDL-@DSP
    IF      @PGMEC    ^=   0    THEN
            CALL SCVMSG1.TOKELIB,PARA-('SORT ERR            ')
            GOTO ABEND
    END

/.################################################################./
/.# ＡＣＯＳ計上Ｆ　ダブリチェック                               #./
/.################################################################./
STEP8:

    CALL SCVMSG1.TOKELIB,PARA-('STEP8=> DTﾀﾞﾌﾞﾘCHK  ')

    OVRF      FILE-ACOS,TOFILE-ACOSWK.TOKWLIB
    OVRF      FILE-ACOSOK,TOFILE-ACOSFILE.TOKWLIB
    OVRF      FILE-ACOSERR,TOFILE-ACOSERR.TOKWLIB
    CALL      PGM-SKY3101B.TOKELIB
    IF        @PGMEC    ^=   0
              THEN
              CALL SCVMSG1.TOKELIB,PARA-('DTﾀﾞﾌﾞﾘCHK ERR      ')
              GOTO ABEND
    END

/.################################################################./
/.# ＡＣＯＳ計上Ｆ　件数リスト                                   #./
/.################################################################./
STEP9:

    CALL SCVMSG1.TOKELIB,PARA-('STEP9=> ｹﾝｽｳﾘｽﾄ     ')

    OVRF      FILE-ACOS,TOFILE-ACOSFILE.TOKWLIB
    CALL      PGM-SKY2901L.TOKELIB
    IF        @PGMEC    ^=   0
              THEN
              CALL SCVMSG1.TOKELIB,PARA-('ｹﾝｽｳﾘｽﾄ ﾊｯｺｳ ERR    ')
              GOTO ABEND
    END

/.################################################################./
/.# 計上データ退避処理                                           #./
/.################################################################./
STEP10:

    CALL SCVMSG1.TOKELIB,PARA-('STEP10=> SAVFILE    ')

    SAVFILE FILE-ACOSWK.TOKWLIB/ACOSFILE.TOKWLIB,TODEV-LTO,
            MODE-@USED
    IF        @PGMEC    ^=   0
              THEN
              CALL SCVMSG1.TOKELIB,PARA-('SAVFILE ERR         ')
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