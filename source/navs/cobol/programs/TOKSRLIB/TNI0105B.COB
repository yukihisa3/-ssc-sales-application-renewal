****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　日次更新処理　　　　　　　　　    *
*    業務名　　　　　　　：　Ｄ３６５受注連携　　              *
*    モジュール名　　　　：　日次計上ＤＴ作成（連携ＤＴ）　　　*
*    作成日／更新日　　　：　2020/04/27                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　売上受注売上調整ＤＴを読み、Ｄ３  *
*                            ６５への連携データを作成する。    *
*    作成日／更新日　　　：　2020/11/20                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　店舗ＣＤ６桁セット　　　　　　　  *
*    作成日／更新日　　　：　2020/12/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　顧客ＩＤ，需要家ＩＤ取得セット    *
*    作成日／更新日　　　：　2020/12/21                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　直送区分セット変更（０固定）　　  *
*    作成日／更新日　　　：　2021/01/12                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　値引伝票対応　　　　　　　　　　  *
*    作成日／更新日　　　：　2021/04/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　明細備考対応　　　　　　　　　　  *
*    作成日／更新日　　　：　2021/06/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　生産者需要家ＩＤセット変更　　　  *
*    作成日／更新日　　　：　2021/06/22                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　ＹＪＰ様依頼日付セット方法変更　  *
*    作成日／更新日　　　：　2022/02/25                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　受注売上値引件数セット追加　　　  *
*    作成日／更新日　　　：　2022/02/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　先行時、実納品日をセット　　　　  *
*    作成日／更新日　　　：　2022/05/02                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　ケーヨー手書時、指定伝票ＮＯセット*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           TNI0105B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2020/04/27.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受注売上調整計上データ
     SELECT   URIKEJF   ASSIGN    TO        DA-01-VI-URIKEJL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KEJ-F48   KEJ-F49
                                            KEJ-F01   KEJ-F02
                                            KEJ-F04   KEJ-F03
                                            KEJ-F05   KEJ-F06
                                            KEJ-F07
                        FILE  STATUS   IS   KEJ-ST.
*Ｄ３６５送受信日次件数ファイル
     SELECT   DNITKNF   ASSIGN    TO        DA-01-VI-DNITKNL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DNI-F01   DNI-F02
                        FILE  STATUS   IS   DNI-ST.
*受注連携データ
     SELECT   SNDNJYTF  ASSIGN    TO        DA-01-S-SNDUJYTF
                        FILE  STATUS   IS   JYT-ST.
*売上連携データ
     SELECT   SNDURIF   ASSIGN    TO        DA-01-S-SNDURIF
                        FILE  STATUS   IS   URI-ST.
*売上調整連携データ
     SELECT   SNDAKKF   ASSIGN    TO        DA-01-S-SNDAKKF
                        FILE  STATUS   IS   JYT-ST.
*
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-ST.
*
*倉庫マスタ
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE  STATUS   IS   SOK-ST.
*#2020/12/01 NAV ST
*顧客需要家ＩＤ管理マスタ
     SELECT   KYKJYKF   ASSIGN    TO        DA-01-VI-KYKJYKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KYK-F01
                                            KYK-F02
                        FILE  STATUS   IS   KYK-ST.
*#2020/12/01 NAV ED
*#2021/01/12 NAV ST
*値引受注連携データ
     SELECT   SNDJNBKF  ASSIGN    TO        DA-01-S-SNDJNBKF
                        FILE  STATUS   IS   JNK-ST.
*値引売上連携データ
     SELECT   SNDUNBKF  ASSIGN    TO        DA-01-S-SNDUNBKF
                        FILE  STATUS   IS   UNK-ST.
*#2021/01/12 NAV ED
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    受注売上調整計上データ　　                               *
****************************************************************
 FD  URIKEJF
                       LABEL     RECORD    IS   STANDARD.
     COPY     URIKEJF   OF        XFDLIB
              JOINING   KEJ  AS   PREFIX.
*
******************************************************************
*    Ｄ３６５送受信日次件数ファイル
******************************************************************
 FD  DNITKNF
                        LABEL RECORD   IS   STANDARD.
     COPY     DNITKNF   OF        XFDLIB
              JOINING   DNI  AS   PREFIX.
*
******************************************************************
*    受注連携データ
******************************************************************
 FD  SNDNJYTF           BLOCK     CONTAINS  1    RECORDS
                        LABEL RECORD   IS   STANDARD.
                        COPY     SNDNJYTF  OF        XFDLIB
                        JOINING   JYT       PREFIX.
*
******************************************************************
*    売上連携データ
******************************************************************
 FD  SNDURIF            BLOCK     CONTAINS  1    RECORDS
                        LABEL RECORD   IS   STANDARD.
                        COPY     SNDURIF   OF        XFDLIB
                        JOINING   URI       PREFIX.
*
******************************************************************
*    売上調整連携データ
******************************************************************
 FD  SNDAKKF            BLOCK     CONTAINS  1    RECORDS
                        LABEL RECORD   IS   STANDARD.
                        COPY     SNDAKKF   OF        XFDLIB
                        JOINING   AKK       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
******************************************************************
*    倉庫マスタ　　　
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*#2020/12/01 NAV ST
******************************************************************
*    顧客需要家ＩＤ管理マスタ
******************************************************************
 FD  KYKJYKF            LABEL RECORD   IS   STANDARD.
     COPY     KYKJYKF   OF        XFDLIB
              JOINING   KYK       PREFIX.
*#2020/12/01 NAV ED
*#2021/01/12 NAV ST
******************************************************************
*    値引受注連携データ
******************************************************************
 FD  SNDJNBKF           BLOCK     CONTAINS  1    RECORDS
                        LABEL RECORD   IS   STANDARD.
                        COPY     SNDNJYTF  OF        XFDLIB
                        JOINING   JNK       PREFIX.
*
******************************************************************
*    値引売上連携データ
******************************************************************
 FD  SNDUNBKF           BLOCK     CONTAINS  1    RECORDS
                        LABEL RECORD   IS   STANDARD.
                        COPY     SNDURIF   OF        XFDLIB
                        JOINING   UNK       PREFIX.
*
*
*************C**************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(03)     VALUE   SPACE.
 01  SHTDENF-INV-FLG           PIC  X(03)     VALUE   SPACE.
 01  SUBTBLF-INV-FLG           PIC  X(03)     VALUE   SPACE.
 01  ZSOKMS-INV-FLG            PIC  X(03)     VALUE   SPACE.
 01  HJYOKEN-INV-FLG           PIC  X(03)     VALUE   SPACE.
*#2020/12/01 NAV ST
 01  KYKJYKF-INV-FLG           PIC  X(03)     VALUE   SPACE.
*#2020/12/01 NAV ED
*#2022/02/25 NAV ST
 01  DNITKNF-INV-FLG           PIC  X(03)     VALUE   SPACE.
*#2022/02/25 NAV ED
 01  READ-CNT                  PIC  9(07)     VALUE   ZERO.
 01  WK-RENBAN                 PIC  X(09)     VALUE   SPACE.
 01  KENSU-CNT.
     03  JYUTYU-WT-CNT         PIC  9(07)     VALUE   ZERO.
     03  URIAGE-WT-CNT         PIC  9(07)     VALUE   ZERO.
     03  TYOUSEI-WT-CNT        PIC  9(07)     VALUE   ZERO.
*#2021/01/12 NAV ST
     03  JNK-WT-CNT            PIC  9(07)     VALUE   ZERO.
     03  UNK-WT-CNT            PIC  9(07)     VALUE   ZERO.
*#2021/01/12 NAV ED
     03  WK-JIKO-NO            PIC  9(07)     VALUE   ZERO.
***** 店舗CD変換
 01  WK-TENPO-CD             PIC   X(06).
 01  WK-TENPO-CD-R           REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-HEN        PIC   9(06).
*ステータス領域
 01  STATUS-AREA.
     03  KEJ-ST                PIC  X(02).
     03  DNI-ST                PIC  X(02).
     03  JYT-ST                PIC  X(02).
     03  URI-ST                PIC  X(02).
     03  AKK-ST                PIC  X(02).
     03  JYO-ST                PIC  X(02).
     03  SOK-ST                PIC  X(02).
*#2020/12/01 NAV ST
     03  KYK-ST                PIC  X(02).
*#2020/12/01 NAV ED
*#2021/01/12 NAV ST
     03  JNK-ST                PIC  X(02).
     03  UNK-ST                PIC  X(02).
*#2021/01/12 NAV ED
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEJ-ERR           PIC N(15) VALUE
         NC"受注売上調整計上ＤＴエラー".
     03  DNI-ERR           PIC N(15) VALUE
         NC"Ｄ３６５送受信日次件数Ｆエラー".
     03  JYT-ERR           PIC N(15) VALUE
         NC"受注計上データエラー".
     03  URI-ERR           PIC N(15) VALUE
         NC"売上計上データエラー".
     03  AKK-ERR           PIC N(15) VALUE
         NC"売上調整計上データエラー".
     03  JYO-ERR           PIC N(15) VALUE
         NC"条件ファイルエラー".
     03  SOK-ERR           PIC N(15) VALUE
         NC"倉庫マスタエラー".
*#2020/12/01 NAV ST
     03  KYK-ERR           PIC N(15) VALUE
         NC"顧客需要家ＩＤ管理マスタエラー".
*#2020/12/01 NAV ED
*#2021/01/12 NAV ST
     03  JNK-ERR           PIC N(15) VALUE
         NC"値引受注連携データエラー".
     03  UNK-ERR           PIC N(15) VALUE
         NC"値引売上連携データエラー".
*#2021/01/12 NAV ED
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
    03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*   日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*売上調整データの赤伝票を退避する。
 01  WK-AKADEN-WORK.
     03  IX                 PIC   9(02).
     03  IY                 PIC   9(02).
     03  WK-AKADEN-FLG      PIC   X(01).
     03  WK-AKADEN-BAK      PIC   X(200)  OCCURS  50.
 01  WK-BREAK-KEY.
     03  WK-KEJ-F48         PIC   9(08).
     03  WK-KEJ-F49         PIC   9(06).
     03  WK-KEJ-F01         PIC   9(08).
     03  WK-KEJ-F02         PIC   9(09).
     03  WK-KEJ-F04         PIC   9(02).
     03  WK-KEJ-F03         PIC   9(01).
     03  WK-KEJ-F05         PIC   9(05).
     03  WK-KEJ-F06         PIC   9(08).
     03  WK-KEJ-F07         PIC   9(02).
*
 LINKAGE              SECTION.
 01   LINK-IN-DATE        PIC  9(08).
 01   LINK-IN-TIME        PIC  9(06).
 01   LINK-OUT-NO         PIC  9(07).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-IN-DATE
                                        LINK-IN-TIME
                                        LINK-OUT-NO.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE URIKEJF.
     MOVE        KEJ-ST    TO        E-ST.
     MOVE        "URIKEJL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEJ-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNITKNF.
     MOVE        DNI-ST     TO       E-ST.
     MOVE        "DNITKNL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DNI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDNJYTF.
     MOVE        JYT-ST     TO       E-ST.
     MOVE        "SNDNJYTF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC4              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDURIF.
     MOVE        URI-ST     TO       E-ST.
     MOVE        "SNDURIF " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     URI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC5              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDAKKF.
     MOVE        AKK-ST     TO       E-ST.
     MOVE        "SNDAKKF " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     AKK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC6              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST     TO       E-ST.
     MOVE        "JYOKEN1 " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC7              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST     TO       E-ST.
     MOVE        "ZSOKMS1 " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC8              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKF.
     MOVE        KYK-ST     TO       E-ST.
     MOVE        "KYKJYKL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KYK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*#2021/01/12 NAV ST
 FILEERR-SEC9              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDJNBKF.
     MOVE        JNK-ST     TO       E-ST.
     MOVE        "SNDJNBKF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JNK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC10             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDUNBKF.
     MOVE        UNK-ST     TO       E-ST.
     MOVE        "SNDUNBKF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     UNK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*#2021/01/12 NAV ED
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC
               UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*実行ＮＯ取得
     PERFORM  JIKONO-SEC.
     DISPLAY NC"＃実行ＮＯ＝" WK-JIKO-NO UPON CONS.
**** MOVE     WK-JIKO-NO        TO    LINK-OUT-NO.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*ファイルのＯＰＥＮ
     OPEN      I-O    URIKEJF  DNITKNF.
     OPEN      INPUT  ZSOKMS   HJYOKEN.
     OPEN      EXTEND SNDNJYTF  SNDURIF  SNDAKKF.
*#2020/12/01 NAV ST
     OPEN      INPUT  KYKJYKF.
*#2020/12/01 NAV ED
*#2021/01/12 NAV ST
     OPEN      EXTEND SNDJNBKF  SNDUNBKF.
*#2021/01/12 NAV ED
*
     DISPLAY "#" NC"日次日付" " = " LINK-IN-DATE "#" UPON CONS.
     DISPLAY "#" NC"日次時刻" " = " LINK-IN-TIME "  #"
                                                      UPON CONS.
*件数初期化
     MOVE     ZERO              TO    KENSU-CNT.
     MOVE     ZERO              TO    WK-BREAK-KEY.
*受注売上調整計上データスタート
     MOVE     SPACE             TO    KEJ-REC.
     INITIALIZE                       KEJ-REC.
     MOVE     LINK-IN-DATE      TO    KEJ-F48.
     MOVE     LINK-IN-TIME      TO    KEJ-F49.
     START  URIKEJF KEY IS  >=  KEJ-F48  KEJ-F49  KEJ-F01
                                KEJ-F02  KEJ-F04  KEJ-F03
                                KEJ-F05  KEJ-F06  KEJ-F07
            INVALID
            MOVE  "END"         TO    END-FLG
            DISPLAY NC"＃＃対象データ無し（ＳＴ）＃＃" UPON CONS
            GO                  TO    INIT-EXIT
     END-START.
*
     PERFORM  URIKEJF-READ-SEC.
     IF   END-FLG =  "END"
          DISPLAY NC"＃＃対象データ無し（ＲＤ）＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    受注売上調整計上データ
****************************************************************
 URIKEJF-READ-SEC     SECTION.
     MOVE     "URIKEJF-READ-SEC"     TO   S-NAME.
*
     READ  URIKEJF   NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   URIKEJF-READ-EXIT
     END-READ.
*
     ADD   1                 TO   READ-CNT
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "## READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*実行日付、実行時刻チェック
     IF   LINK-IN-DATE  =  KEJ-F48
     AND  LINK-IN-TIME  =  KEJ-F49
          CONTINUE
     ELSE
           MOVE   "END"      TO   END-FLG
     END-IF.
*
 URIKEJF-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
     MOVE     "ZSOKMS-READ-SREC"      TO   S-NAME.
*
     READ  ZSOKMS
           INVALID     MOVE  "INV"    TO   ZSOKMS-INV-FLG
           NOT INVALID MOVE  SPACE    TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込　　　
****************************************************************
 JIKONO-SEC            SECTION.
     MOVE     "JIKONO-SEC"            TO   S-NAME.
*
     OPEN  I-O  HJYOKEN.
*
     MOVE   99                        TO   JYO-F01.
     MOVE  "D36501"                   TO   JYO-F02.
     READ  HJYOKEN
           INVALID     MOVE  "INV"    TO   HJYOKEN-INV-FLG
           NOT INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
     IF  HJYOKEN-INV-FLG  =  "INV"
         DISPLAY NC"＃実行ＮＯ取得異常！＃" UPON CONS
         MOVE    4000                 TO   PROGRAM-STATUS
         STOP  RUN
     ELSE
         ADD     1                    TO   JYO-F04
         IF  JYO-F04  >  JYO-F06
             MOVE   JYO-F05           TO   JYO-F04
         END-IF
         MOVE    JYO-F04              TO   WK-JIKO-NO
         REWRITE  JYO-REC
     END-IF.
*
     CLOSE  HJYOKEN.
*
 JIKONO-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込　　　
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
     MOVE     "HJYOKEN-READ-SEC"      TO   S-NAME.
*
     READ  HJYOKEN
           INVALID     MOVE  "INV"    TO   HJYOKEN-INV-FLG
           NOT INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*#2020/12/01 NAV ST
****************************************************************
*    顧客需要家ＩＤ管理マスタ読込
****************************************************************
 KYKJYKF-READ-SEC      SECTION.
     MOVE     "KYKJYKF-READ-SEC"      TO   S-NAME.
*
     READ  KYKJYKF
           INVALID     MOVE  "INV"    TO   KYKJYKF-INV-FLG
           NOT INVALID MOVE  SPACE    TO   KYKJYKF-INV-FLG
     END-READ.
*
 KYKJYKF-READ-EXIT.
     EXIT.
*#2020/12/01 NAV ED
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     IF   WK-KEJ-F48  =  KEJ-F48
     AND  WK-KEJ-F49  =  KEJ-F49
     AND  WK-KEJ-F01  =  KEJ-F01
     AND  WK-KEJ-F02  =  KEJ-F02
     AND  WK-KEJ-F04  =  KEJ-F04
     AND  WK-KEJ-F03  =  KEJ-F03
     AND  WK-KEJ-F05  =  KEJ-F05
     AND  WK-KEJ-F06  =  KEJ-F06
*****AND  WK-KEJ-F07  =  KEJ-F07
          CONTINUE
     ELSE
*******   DISPLAY "WK-AKADEN-FLG = " WK-AKADEN-FLG UPON CONS
*******   DISPLAY "KEJ-F03       = " KEJ-F03       UPON CONS
          IF   WK-AKADEN-FLG  = "1"
               IF  KEJ-F03  =  2  OR  4  OR  6
                   CONTINUE
               ELSE
                   PERFORM AKAKURO-SEC
               END-IF
          END-IF
          INITIALIZE          WK-AKADEN-WORK
          MOVE   KEJ-F48  TO  WK-KEJ-F48
          MOVE   KEJ-F49  TO  WK-KEJ-F49
          MOVE   KEJ-F01  TO  WK-KEJ-F01
          MOVE   KEJ-F02  TO  WK-KEJ-F02
          MOVE   KEJ-F04  TO  WK-KEJ-F04
          MOVE   KEJ-F03  TO  WK-KEJ-F03
          MOVE   KEJ-F05  TO  WK-KEJ-F05
          MOVE   KEJ-F06  TO  WK-KEJ-F06
**********MOVE   KEJ-F07  TO  WK-KEJ-F07
     END-IF.
*判定
*受注データ作成判定
     IF  KEJ-F39  =  "1"  OR  "2"
         PERFORM  JYUTYU-WT-SEC
         MOVE     "1"      TO   KEJ-F40
         MOVE    SYS-DATEW TO   KEJ-F41
     END-IF.
*売上データ作成判定
     IF  KEJ-F42  =  "1"  OR  "2"
         PERFORM  URIAGE-WT-SEC
         MOVE     "1"      TO   KEJ-F43
         MOVE    SYS-DATEW TO   KEJ-F44
     END-IF.
*売上調整データ作成判定
     IF  KEJ-F45  =  "1"  OR  "2"
         PERFORM  TYOUSEI-WT-SEC
         MOVE     "1"      TO   KEJ-F46
         MOVE    SYS-DATEW TO   KEJ-F47
     END-IF.
*
     REWRITE  KEJ-REC.
*
     PERFORM  URIKEJF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    受注データ作成
****************************************************************
 JYUTYU-WT-SEC         SECTION.
*
     MOVE    "JYUTYU-WT-SEC"          TO   S-NAME.
*初期化
     MOVE     SPACE                   TO   JYT-REC.
     INITIALIZE                            JYT-REC.
*受注連携データ出力
*    Ｄ３６５伝票番号
     MOVE      KEJ-F09                TO   JYT-F01.
*    オーダー区分
     MOVE      KEJ-F08                TO   JYT-F04.
*    受注日
*#2011/06/22 NAV ST ＹＪＰ様指示　仕様変更（出荷日セット）
*****MOVE      KEJ-F12(1:4)           TO   JYT-F05(1:4).
*    MOVE      "/"                    TO   JYT-F05(5:1).
*    MOVE      KEJ-F12(5:2)           TO   JYT-F05(6:2).
*    MOVE      "/"                    TO   JYT-F05(8:1).
*****MOVE      KEJ-F12(7:2)           TO   JYT-F05(9:2).
     MOVE      KEJ-F13(1:4)           TO   JYT-F05(1:4).
     MOVE      "/"                    TO   JYT-F05(5:1).
     MOVE      KEJ-F13(5:2)           TO   JYT-F05(6:2).
     MOVE      "/"                    TO   JYT-F05(8:1).
     MOVE      KEJ-F13(7:2)           TO   JYT-F05(9:2).
*#2021/06/22 NAV ED
*#2022/02/28 NAV ST 実納品日対応
     IF   KEJ-F34  =  "1"
          IF  KEJ-F51  NUMERIC
          AND KEJ-F51  >  ZERO
              MOVE KEJ-F51(1:4)       TO   JYT-F05(1:4)
              MOVE "/"                TO   JYT-F05(5:1)
              MOVE KEJ-F51(5:2)       TO   JYT-F05(6:2)
              MOVE "/"                TO   JYT-F05(8:1)
              MOVE KEJ-F51(7:2)       TO   JYT-F05(9:2)
          END-IF
     END-IF.
*#2022/02/28 NAV ED
*    販売価格設定
     MOVE      "1"                    TO   JYT-F10.
*    倉庫　　　　　　　
     MOVE      KEJ-F11                TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "SSC"                  TO   JYT-F11
          MOVE "0"                    TO   JYT-F12(1:1)
          MOVE  KEJ-F11               TO   JYT-F12(2:2)
          MOVE  "00"                  TO   JYT-F13
     ELSE
**********サイト取得
          MOVE  41                    TO   JYO-F01
          MOVE  SOK-F15               TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "SSC"             TO   JYT-F11
          ELSE
              MOVE   JYO-F14          TO   JYT-F11
          END-IF
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16               TO   JYT-F12
**********保管場所取得
          MOVE  42                    TO   JYO-F01
          MOVE  SOK-F17               TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "11"              TO   JYT-F13
          ELSE
              MOVE   JYO-F14          TO   JYT-F13
          END-IF
     END-IF.
*    出荷予定日
     MOVE    "6"                      TO   LINK-IN-KBN.
     MOVE     1                       TO   LINK-IN-YMD6.
     MOVE     KEJ-F06                 TO   LINK-IN-YMD8.
     CALL    "SKYDTCKB" USING              LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
*****         MOVE     LINK-OUT-YMD8 TO   SYS-DATEW
              CONTINUE
     ELSE
*****         MOVE    ZERO           TO   SYS-DATEW
              MOVE    KEJ-F06        TO   LINK-OUT-YMD8
     END-IF.
*#2011/06/22 NAV ST ＹＪＰ様指示　仕様変更（出荷日セット）
*****MOVE      LINK-OUT-YMD8(1:4)    TO   JYT-F14(1:4).
*    MOVE      "/"                   TO   JYT-F14(5:1).
*    MOVE      LINK-OUT-YMD8(5:2)    TO   JYT-F14(6:2).
*    MOVE      "/"                   TO   JYT-F14(8:1).
*****MOVE      LINK-OUT-YMD8(7:2)    TO   JYT-F14(9:2).
     MOVE      KEJ-F13(1:4)          TO   JYT-F14(1:4).
     MOVE      "/"                   TO   JYT-F14(5:1).
     MOVE      KEJ-F13(5:2)          TO   JYT-F14(6:2).
     MOVE      "/"                   TO   JYT-F14(8:1).
     MOVE      KEJ-F13(7:2)          TO   JYT-F14(9:2).
*#2022/02/28 NAV ST 実納品日対応
     IF   KEJ-F34  =  "1"
          IF  KEJ-F51  NUMERIC
          AND KEJ-F51  >  ZERO
              MOVE KEJ-F51(1:4)      TO   JYT-F14(1:4)
              MOVE "/"               TO   JYT-F14(5:1)
              MOVE KEJ-F51(5:2)      TO   JYT-F14(6:2)
              MOVE "/"               TO   JYT-F14(8:1)
              MOVE KEJ-F51(7:2)      TO   JYT-F14(9:2)
          END-IF
     END-IF.
*#2011/06/22 NAV ED
*    入荷希望日
*#2011/06/22 NAV ST ＹＪＰ様指示　仕様変更（出荷日セット）
*****MOVE      KEJ-F06(1:4)          TO   JYT-F15(1:4).
*    MOVE      "/"                   TO   JYT-F15(5:1).
*    MOVE      KEJ-F06(5:2)          TO   JYT-F15(6:2).
*    MOVE      "/"                   TO   JYT-F15(8:1).
*****MOVE      KEJ-F06(7:2)          TO   JYT-F15(9:2).
     MOVE      KEJ-F13(1:4)          TO   JYT-F15(1:4).
     MOVE      "/"                   TO   JYT-F15(5:1).
     MOVE      KEJ-F13(5:2)          TO   JYT-F15(6:2).
     MOVE      "/"                   TO   JYT-F15(8:1).
     MOVE      KEJ-F13(7:2)          TO   JYT-F15(9:2).
*#2011/06/22 NAV ED
*#2022/02/28 NAV ST 実納品日対応
     IF   KEJ-F34  =  "1"
          IF  KEJ-F51  NUMERIC
          AND KEJ-F51  >  ZERO
              MOVE KEJ-F51(1:4)      TO   JYT-F15(1:4)
              MOVE "/"               TO   JYT-F15(5:1)
              MOVE KEJ-F51(5:2)      TO   JYT-F15(6:2)
              MOVE "/"               TO   JYT-F15(8:1)
              MOVE KEJ-F51(7:2)      TO   JYT-F15(9:2)
          END-IF
     END-IF.
*#2022/02/28 NAV ED
*    顧客発注番号
     MOVE      KEJ-F02                TO   JYT-F18.
*****2022/05/02 NAV ST ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
     IF  KEJ-F01  =  1731 OR 1732 OR 7601 OR 7602
         IF  KEJ-F02  NOT =  KEJ-F26
             MOVE    KEJ-F26          TO   JYT-F18
         END-IF
     END-IF.
*****2022/05/02 NAV ED ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
*    外部システム番号
     MOVE      KEJ-F09                TO   JYT-F19.
*    連携ＮＯ
     MOVE      SPACE                  TO   JYT-F20.
*    連携データ区分
     MOVE      "1"                    TO   JYT-F21.
*#2020/12/01 NAV ST
*顧客需要家ＩＤ管理マスタ索引
     MOVE    KEJ-F01                   TO   KYK-F01.
     MOVE    KEJ-F05                   TO   KYK-F02.
     PERFORM KYKJYKF-READ-SEC.
     IF   KYKJYKF-INV-FLG  =  "INV"
          DISPLAY "# " NC"顧客需要家ＩＤ異常" ":"
                       NC"取引先" " = " KEJ-F01   "/"
                       NC"店　舗" " = " KEJ-F05   "/"
                       NC"伝票" "NO = " KEJ-F02   "/"
                       NC"納品日" " = " KEJ-F06   " #"
**********MOVE 4000                    TO   PROGRAM-STATUS
**********STOP  RUN
          GO                           TO   JYUTYU-WT-EXIT
     ELSE *>ＨＤ：顧客ＩＤ、需要家ＩＤセット　ＭＳ：需要家ＩＤ
          MOVE  KYK-F03                TO   JYT-F22(1:10)
          MOVE  KYK-F04                TO   JYT-F24(1:10)
                                            JYT-F61(1:10)
**********#2021/06/01 NAV ST 計上需要家ＩＤがセットされた場合
          IF   KEJ-F50  NOT =  SPACE
               MOVE   KEJ-F50          TO   JYT-F24(1:10)
               MOVE   KEJ-F50          TO   JYT-F61(1:10)
          END-IF
**********#2021/06/01 NAV ED
     END-IF.
*#2020/12/01 NAV ED
*    旧顧客ＣＤ番号
     MOVE      KEJ-F27                TO   JYT-F23.
*    旧納入先ＣＤ
*#2020/11/20 NAV ST
*****MOVE      KEJ-F05                TO   JYT-F25.
     MOVE      KEJ-F05                TO   WK-TENPO-HEN.
     MOVE      WK-TENPO-CD            TO   JYT-F25.
*#2020/11/20 NAV ED
*    一時出荷先フラグ
     MOVE      "0"                    TO   JYT-F26.
*    旧商品ＣＤ
*    ＪＡＮＣＤ
     MOVE      KEJ-F282               TO   JYT-F46.
*    数量符号
     MOVE      SPACE                  TO   JYT-F470.
*#2020/06/29 NAV ST 連携課題対応
*****IF   KEJ-F04  =  41
*****受注連携対象区分で判断
     IF   KEJ-F39  =  "2"
          MOVE  "-"                   TO   JYT-F470
     ELSE
          MOVE  "0"                   TO   JYT-F470
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    数量（×１００）
     MOVE      KEJ-F18                TO   JYT-F47.
     COMPUTE  JYT-F47  =  KEJ-F18  *  100.
*    直送フラグ
     MOVE      "0"                    TO   JYT-F51.
*#2020/12/21 NAV ST 日次内から作成される受注はすべて直送
*****IF  JYT-F04(1:3) = "D23" *>直送の場合
*        MOVE  "1"                    TO   JYT-F51
*****END-IF.
*#2020/12/21 NAV ED
*    販売価格設定
     MOVE      "1"                    TO   JYT-F57.
*    販売単価（×１００）
     MOVE      KEJ-F21                TO   JYT-F58.
     COMPUTE  JYT-F58  =  KEJ-F21  *  100.
*    受注金額符号
*****MOVE      SPACE                  TO   JYT-F590.
*#2020/06/29 NAV ST 連携課題対応
*****IF   KEJ-F04  =  41
*****受注連携対象区分で判断
     IF   KEJ-F39  =  "2"
          MOVE  "-"                   TO   JYT-F590
     ELSE
          MOVE  "0"                   TO   JYT-F590
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    受注金額
     MOVE      KEJ-F23                TO   JYT-F59.
     COMPUTE  JYT-F59  =  KEJ-F23  *  100.
*    販売価格単位
     MOVE      "1"                    TO   JYT-F60.
*    旧納入先ＣＤ
*#2020/11/20 NAV ST
*****MOVE      KEJ-F05                TO   JYT-F62.
     MOVE      WK-TENPO-CD            TO   JYT-F62.
*#2020/11/20 NAV ED
*    出荷先フラグ
     MOVE      "0"                    TO   JYT-F63.
*    外部システム番号
     MOVE      KEJ-F09                TO   JYT-F77.
*    外部システム行番号
     MOVE      KEJ-F07                TO   JYT-F78.
*#2021/04/28 NAV ST 備考（受注用）を追加
*    備考（受注用）
     MOVE      KEJ-F25                TO   JYT-F74.
*#2021/04/28 NAV ED
*
*#2021/01/12 NAV ST
* 値引伝票の場合、値引用の受注連携ファイルへ出力する様に変更
*****WRITE  JYT-REC.
*****ADD       1                      TO   JYUTYU-WT-CNT.
     IF   KEJ-F04  NOT =  "42"
          WRITE  JYT-REC
          ADD    1                    TO   JYUTYU-WT-CNT
     ELSE
          MOVE   SPACE                TO   JNK-REC
          INITIALIZE                       JNK-REC
          MOVE   JYT-REC              TO   JNK-REC
          WRITE  JNK-REC
          ADD    1                    TO   JNK-WT-CNT
     END-IF.
*#2021/01/12 NAV ED
*
 JYUTYU-WT-EXIT.
     EXIT.
****************************************************************
*    売上データ作成
****************************************************************
 URIAGE-WT-SEC         SECTION.
*
     MOVE     "URIAGE-WT-SEC"         TO   S-NAME.
*初期化
     MOVE     SPACE                   TO   URI-REC.
     INITIALIZE                            URI-REC.
*受注連携データ出力
*    Ｄ３６５伝票番号
     MOVE      KEJ-F09                TO   URI-F01.
*    行番号
     MOVE      KEJ-F07                TO   URI-F02.
*    連携ＮＯ
     MOVE      KEJ-F35                TO   URI-F03.
*    顧客発注ＮＯ　　　
     MOVE      KEJ-F02                TO   URI-F04.
*****2022/05/02 NAV ST ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
     IF  KEJ-F01  =  1731 OR 1732 OR 7601 OR 7602
         IF  KEJ-F02  NOT =  KEJ-F26
             MOVE    KEJ-F26          TO   URI-F04
         END-IF
     END-IF.
*****2022/05/02 NAV ED ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
*    売上日
     MOVE      KEJ-F13(1:4)           TO   URI-F05(1:4).
     MOVE      "/"                    TO   URI-F05(5:1).
     MOVE      KEJ-F13(5:2)           TO   URI-F05(6:2).
     MOVE      "/"                    TO   URI-F05(8:1).
     MOVE      KEJ-F13(7:2)           TO   URI-F05(9:2).
*#2022/02/28 NAV ST 実納品日対応
     IF   KEJ-F34  =  "1"
          IF  KEJ-F51  NUMERIC
          AND KEJ-F51  >  ZERO
              MOVE KEJ-F51(1:4)       TO   URI-F05(1:4)
              MOVE "/"                TO   URI-F05(5:1)
              MOVE KEJ-F51(5:2)       TO   URI-F05(6:2)
              MOVE "/"                TO   URI-F05(8:1)
              MOVE KEJ-F51(7:2)       TO   URI-F05(9:2)
          END-IF
     END-IF.
*#2022/02/28 NAV ED
*    新商品ＣＤ
     MOVE      SPACE                  TO   URI-F06.
*    旧商品ＣＤ
     MOVE      SPACE                  TO   URI-F07.
*    ＪＡＮＣＤ
     MOVE      KEJ-F282               TO   URI-F08.
*    数量符号
     MOVE      "0"                    TO   URI-F09.
*#2020/06/29 NAV ST 連携課題対応
*****IF   KEJ-F04  = 41
*****売上連携対象区分で判断
     IF   KEJ-F42  = "2"
          MOVE  "-"                   TO   URI-F09
     ELSE
          MOVE  "0"                   TO   URI-F09
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    数量
     MOVE      KEJ-F18                TO   URI-F10.
     COMPUTE  URI-F10  =  KEJ-F18  *  100.
*    単価
     MOVE      KEJ-F21                TO   URI-F11.
     COMPUTE  URI-F11  =  KEJ-F21  *  100.
*    数量符号
*#2020/06/29 NAV ST 連携課題対応
*****IF   KEJ-F04  = 41
*****売上連携対象区分で判断
     IF   KEJ-F42  = "2"
          MOVE  "-"                   TO   URI-F12
     ELSE
          MOVE  "0"                   TO   URI-F12
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    金額
     MOVE      KEJ-F23                TO   URI-F13.
     COMPUTE  URI-F13  =  KEJ-F23  *  100.
*    税区分
     MOVE      KEJ-F36                TO   URI-F14.
*
*#2021/01/12 NAV ST
* 値引伝票の場合、値引用の売上連携ファイルへ出力する様に変更
*****WRITE  URI-REC.
*****ADD       1                      TO   URIAGE-WT-CNT.
     IF     KEJ-F04  NOT =  "42"
            WRITE  URI-REC
            ADD    1                  TO   URIAGE-WT-CNT
     ELSE
            MOVE   SPACE              TO   UNK-REC
            INITIALIZE                     UNK-REC
            MOVE   URI-REC            TO   UNK-REC
            WRITE  UNK-REC
            ADD    1                  TO   UNK-WT-CNT
     END-IF.
*#2021/01/12 NAV ED
*
 URIAGE-WT-EXIT.
     EXIT.
****************************************************************
*    売上調整データ作成
****************************************************************
 TYOUSEI-WT-SEC        SECTION.
*
     MOVE     "TYOUSEI-WT-SEC"        TO   S-NAME.
*初期化
     MOVE     SPACE                   TO   AKK-REC.
     INITIALIZE                            AKK-REC.
*受注連携データ出力
*    Ｄ３６５伝票番号
     MOVE      KEJ-F09                TO   AKK-F01.
*    行番号
     MOVE      KEJ-F07                TO   AKK-F02.
*    連携ＮＯ
     MOVE      KEJ-F35                TO   AKK-F03.
*    赤黒区分／数量符号
*#2020/06/29 NAV ST 連携課題対応
*****MOVE      KEJ-F35                TO   AKK-F04.
***  IF   KEJ-F45  =  "1"  *>黒伝
***       MOVE "1"                    TO   AKK-F04
***       MOVE "0"                    TO   AKK-F10
***  ELSE                  *>赤伝
***       MOVE "0"                    TO   AKK-F04
***       MOVE "-"                    TO   AKK-F10
***  END-IF.
***  EVALUATE  KEJ-F04  ALSO  KEJ-F45
***      WHEN  40       ALSO  "2"  *>伝区４０、赤伝、０、＋
***            MOVE   "0"             TO   AKK-F04
***            MOVE   "0"             TO   AKK-F10  AKK-F13
***      WHEN  40       ALSO  "1"  *>伝区４０、黒伝、１、＋
***            MOVE   "1"             TO   AKK-F04
***            MOVE   "0"             TO   AKK-F10  AKK-F13
***      WHEN  41       ALSO  "1"  *>伝区４１、赤伝、０、―
***            MOVE   "0"             TO   AKK-F04
***            MOVE   "-"             TO   AKK-F10  AKK-F13
***      WHEN  41       ALSO  "2"  *>伝区４１、黒伝、１、―
***            MOVE   "1"             TO   AKK-F04
***            MOVE   "-"             TO   AKK-F10  AKK-F13
***      WHEN  42       ALSO  "1"  *>伝区４２、赤伝、０、―
***            MOVE   "0"             TO   AKK-F04
***            MOVE   "-"             TO   AKK-F10  AKK-F13
***      WHEN  42       ALSO  "2"  *>伝区４２、黒伝、１、―
***            MOVE   "1"             TO   AKK-F04
***            MOVE   "-"             TO   AKK-F10  AKK-F13
***  END-EVALUATE.
*#2020/06/29 NAV ED 連携課題対応
*#2020/06/29 NAV ST 連携課題対応
*    赤黒区分
     IF   KEJ-F03  =  1 OR 3 OR 5
          MOVE  0                     TO   AKK-F04
     ELSE
          MOVE  1                     TO   AKK-F04
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    顧客発注ＮＯ　　　
     MOVE      KEJ-F02                TO   AKK-F05.
*****2022/05/02 NAV ST ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
     IF  KEJ-F01  =  1731 OR 1732 OR 7601 OR 7602
         IF  KEJ-F02  NOT =  KEJ-F26
             MOVE    KEJ-F26          TO   AKK-F05
         END-IF
     END-IF.
*****2022/05/02 NAV ED ケーヨーで伝票ＮＯ＜＞指定伝票ＮＯの時
*    売上調整日
     MOVE      KEJ-F13(1:4)           TO   AKK-F06(1:4).
     MOVE      "/"                    TO   AKK-F06(5:1).
     MOVE      KEJ-F13(5:2)           TO   AKK-F06(6:2).
     MOVE      "/"                    TO   AKK-F06(8:1).
     MOVE      KEJ-F13(7:2)           TO   AKK-F06(9:2).
*    新商品ＣＤ
     MOVE      SPACE                  TO   AKK-F07.
*    旧商品ＣＤ
     MOVE      SPACE                  TO   AKK-F08.
*    ＪＡＮＣＤ
     MOVE      KEJ-F282               TO   AKK-F09.
*    数量符号
*#2020/06/29 NAV ST 連携課題対応
*****MOVE      "0"                    TO   AKK-F10.
*****売上調整連携対象区分で判断
     IF   KEJ-F45  = "2"
          MOVE  "-"                   TO   AKK-F10
     ELSE
          MOVE  "0"                   TO   AKK-F10
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    数量
     MOVE      KEJ-F18                TO   AKK-F11.
     COMPUTE  AKK-F11  =  KEJ-F18  *  100.
*    単価
     MOVE      KEJ-F21                TO   AKK-F12.
     COMPUTE  AKK-F12  =  KEJ-F21  *  100.
*    金額符号
*#2020/06/29 NAV ST 連携課題対応
*****MOVE      "0"                    TO   AKK-F13.
*****売上調整連携対象区分で判断
     IF   KEJ-F45  = "2"
          MOVE  "-"                   TO   AKK-F13
     ELSE
          MOVE  "0"                   TO   AKK-F13
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*    金額
     MOVE      KEJ-F23                TO   AKK-F14.
     COMPUTE  AKK-F14  =  KEJ-F23  *  100.
*    税区分
     MOVE      KEJ-F36                TO   AKK-F15.
*赤伝の場合、ワークにレコードを退避する。赤黒セットを作成の為
     IF  KEJ-F03  =  1  OR  3  OR  5
*********DISPLAY "AAAAA" UPON CONS
         MOVE  "1"                    TO   WK-AKADEN-FLG
         ADD   1                      TO   IX
         MOVE  AKK-REC                TO   WK-AKADEN-BAK(IX)
     END-IF.
*
     WRITE  AKK-REC.
     ADD       1                      TO   TYOUSEI-WT-CNT.
*
 TYOUSEI-WT-EXIT.
     EXIT.
****************************************************************
*    赤黒対データ作成処理
****************************************************************
 AKAKURO-SEC           SECTION.
*
     MOVE     "AKAKURO-SEC"           TO   S-NAME.
*
*****DISPLAY "BBBBB" UPON  CONS.
     PERFORM  VARYING  IY  FROM  1  BY  1  UNTIL  IX  <  IY
*****DISPLAY "CCCCC" UPON  CONS
        MOVE     SPACE                TO   AKK-REC
        INITIALIZE                         AKK-REC
        MOVE     WK-AKADEN-BAK(IY)    TO   AKK-REC
        MOVE     "1"                  TO   AKK-F04
        MOVE     "0"                  TO   AKK-F10  AKK-F13
        MOVE     ZERO                 TO   AKK-F11  AKK-F14
        WRITE    AKK-REC
        ADD      1                    TO   TYOUSEI-WT-CNT
     END-PERFORM.
*
 AKAKURO-EXIT.
     EXIT.
****************************************************************
*    Ｄ３６５送受信日次件数ファイル
****************************************************************
 D365-KENSU-WT-SEC     SECTION.
*
     MOVE     "D365-KENSU-WT-SEC"     TO   S-NAME.
*#2022/02/25 NAV ST
*****Ｄ３６５送受信日次件数ファイル存在チェック
     MOVE      LINK-IN-DATE           TO   DNI-F01.
     MOVE      LINK-IN-TIME           TO   DNI-F02.
     READ   DNITKNF
            INVALID       MOVE  "INV" TO   DNITKNF-INV-FLG
            NOT  INVALID  MOVE  SPACE TO   DNITKNF-INV-FLG
     END-READ.
*更新判定
     IF     DNITKNF-INV-FLG = "INV"
************初期化
            MOVE     SPACE                   TO   DNI-REC
            INITIALIZE                            DNI-REC
************Ｄ３６５送受信日次件数ファイル出力
*           連携日付
            MOVE      LINK-IN-DATE           TO   DNI-F01
*           連携時刻
            MOVE      LINK-IN-TIME           TO   DNI-F02
*           受注データ作成件数
            MOVE      JYUTYU-WT-CNT          TO   DNI-F03
*           売上データ作成件数
            MOVE      URIAGE-WT-CNT          TO   DNI-F04
*           売上調整データ作成件数
            MOVE      TYOUSEI-WT-CNT         TO   DNI-F05
*************#2022/02/25 NAV ST
*           受注値引データ件数　　　
            MOVE      JNK-WT-CNT             TO   DNI-F06
*           売上値引データ件数
            MOVE      UNK-WT-CNT             TO   DNI-F07
*************#2022/02/25 NAV ED
*           システム日付
            MOVE      SYS-DATEW              TO   DNI-F98
*           システム時刻
            MOVE      SYS-HHMNSS             TO   DNI-F99
*
            WRITE  DNI-REC
     ELSE
************Ｄ３６５送受信日次件数ファイル出力
*           連携日付
            MOVE      LINK-IN-DATE           TO   DNI-F01
*           連携時刻
            MOVE      LINK-IN-TIME           TO   DNI-F02
*           受注データ作成件数
            MOVE      JYUTYU-WT-CNT          TO   DNI-F03
*           売上データ作成件数
            MOVE      URIAGE-WT-CNT          TO   DNI-F04
*           売上調整データ作成件数
            MOVE      TYOUSEI-WT-CNT         TO   DNI-F05
*************#2022/02/25 NAV ST
*           受注値引データ件数　　　
            MOVE      JNK-WT-CNT             TO   DNI-F06
*           売上値引データ件数
            MOVE      UNK-WT-CNT             TO   DNI-F07
*************#2022/02/25 NAV ED
*           システム日付
            MOVE      SYS-DATEW              TO   DNI-F98
*           システム時刻
            MOVE      SYS-HHMNSS             TO   DNI-F99
*
            REWRITE  DNI-REC
     END-IF.
*
 D365-KENSU-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*
     IF   WK-AKADEN-FLG  = "1"
          PERFORM AKAKURO-SEC
     END-IF .
*件数ファイル更新
     PERFORM  D365-KENSU-WT-SEC.
*ファイルのＯＰＥＮ
     CLOSE     URIKEJF  DNITKNF.
     CLOSE     ZSOKMS   HJYOKEN.
     CLOSE     SNDNJYTF  SNDURIF  SNDAKKF.
*#2021/01/12 NAV ST
     CLOSE     KYKJYKF  SNDJNBKF  SNDUNBKF.
*#2021/01/12 NAV ED
*
*#2021/01/12 NAV ST
*****DISPLAY "## READ-CNT = " READ-CNT       " ##" UPON CONS.
*****DISPLAY "## JYT-CNT  = " JYUTYU-WT-CNT  " ##" UPON CONS.
*****DISPLAY "## URI-CNT  = " URIAGE-WT-CNT  " ##" UPON CONS.
*****DISPLAY "## TYS-CNT  = " TYOUSEI-WT-CNT " ##" UPON CONS.
     DISPLAY "## " NC"受注ＤＴ　　" " = " JYUTYU-WT-CNT " ##"
     UPON CONS.
     DISPLAY "## " NC"売上ＤＴ　　" " = " URIAGE-WT-CNT " ##"
     UPON CONS.
     DISPLAY "## " NC"売上調整ＤＴ" " = " TYOUSEI-WT-CNT " ##"
     UPON CONS.
     DISPLAY "## " NC"値引受注ＤＴ" " = " JNK-WT-CNT    " ##"
     UPON CONS.
     DISPLAY "## " NC"値引売上ＤＴ" " = " UNK-WT-CNT    " ##"
     UPON CONS.
*#2021/01/12 NAV ED
*
 END-EXIT.
     EXIT.
