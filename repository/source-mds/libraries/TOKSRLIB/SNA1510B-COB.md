# SNA1510B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SNA1510B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携（Ｄ３６５連携）   *
*    業務名　　　　　　　：　Ｄ３６５受注連携　　              *
*    モジュール名　　　　：　連携ＤＴ＝＞受注ＤＴ作成　　　　　*
*    作成日／更新日　　　：　2020/04/07                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　小売連携発注データより連携ＤＴを  *
*                            作成する。                        *
*    作成日／更新日　　　：　2020/06/03                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　課題対応（在庫調整ＦＬＧのセット）*
*    作成日／更新日　　　：　2020/07/20                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　Ｄ３６５伝票番号にオーダー区分    *
*    作成日／更新日　　　：　2020/10/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　小売連携区分＝３の時、オーダー    *
*                            区分に”Ｄ３１”をセットに変更    *
*    作成日／更新日　　　：　2020/12/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　顧客ＩＤ，需要家ＩＤ取得セット    *
*    作成日／更新日　　　：　2020/12/14                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　オーダー区分セット方法変更　　　  *
*    作成日／更新日　　　：　2020/12/15                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　特定のオーダー区分の場合のサイト  *
*                            倉庫、保管場所、直送のセット変更  *
*    作成日／更新日　　　：　2021/01/05                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　返品データ対応　　　　　　　　　　*
*    作成日／更新日　　　：　2021/04/30                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　明細備考転送対応　　　　　　　　  *
*    作成日／更新日　　　：　2021/05/14                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　見本鉢／無償品対応／サービス対応  *
*    作成日／更新日　　　：　2021/06/24                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　受注、出荷、入荷日付セット変更　  *
*    作成日／更新日　　　：　2021/07/06                        *
*    作成者／更新者　　　：　ＮＡＶ高橋（Ｐ３１，Ｐ４１）      *
*    処理概要　　　　　　：　特定のオーダー区分の場合のサイト  *
*                            倉庫、保管場所、直送のセット変更  *
*    作成日／更新日　　　：　2022/02/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　      *
*    処理概要　　　　　　：　受注日、出荷日、入荷日に実納品日  *
*    　　　　　　　　　　　　をセットに変更する。　　　　　　  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA1510B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2020/04/07.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*小売連携発注データ
     SELECT  EXXXXXXA  ASSIGN TO   DA-01-S-EXXFIL
             ORGANIZAITION         SEQUENTIAL
                         FILE      STATUS    EXX-ST.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-ST.
*商品名称マスタ
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE  STATUS   IS   TBL-ST.
*倉庫マスタ
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE  STATUS   IS   SOK-ST.
*
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-ST.
*#2020/12/01 NAV ST
*顧客需要家ＩＤ管理マスタ
     SELECT   KYKJYKF   ASSIGN    TO        DA-01-VI-KYKJYKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KYK-F01
                                            KYK-F02
                        FILE  STATUS   IS   KYK-ST.
*#2020/12/01 NAV ED
*
*Ｄ３６５受注連携データ
     SELECT  SNDNJYTF    ASSIGN  TO  DA-01-S-SNDNJYTF
                        FILE  STATUS   IS   JYT-ST.
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    小売連携発注データ　　　　                               *
****************************************************************
 FD  EXXXXXXA          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EXXXXXXA  OF   XFDLIB
                       JOINING   EXX       AS   PREFIX.
*
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    サブ商品名称マスタ
******************************************************************
 FD  SUBTBLF            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*#2020/12/01 NAV ST
******************************************************************
*    顧客需要家ＩＤ管理マスタ
******************************************************************
 FD  KYKJYKF            LABEL RECORD   IS   STANDARD.
     COPY     KYKJYKF   OF        XFDLIB
              JOINING   KYK       PREFIX.
*#2020/12/01 NAV ED
****************************************************************
*    Ｄ３６５受注連携データ
****************************************************************
 FD  SNDNJYTF           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SNDNJYTF   OF   XFDLIB
                       JOINING   JYT       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(03)     VALUE   SPACE.
 01  SHTDENF-INV-FLG           PIC  X(03)     VALUE   SPACE.
 01  SUBTBLF-INV-FLG           PIC  X(03)     VALUE   SPACE.
 01  ZSOKMS-INV-FLG            PIC  X(03)     VALUE   SPACE.
 01  HJYOKEN-INV-FLG           PIC  X(03)     VALUE   SPACE.
*#2020/12/01 NAV ST
 01  KYKJYKF-INV-FLG           PIC  X(03)     VALUE   SPACE.
 01  ERR-CNT                   PIC  9(07)     VALUE   ZERO.
*#2020/12/01 NAV ED
 01  WRT-CNT                   PIC  9(07)     VALUE   ZERO.
 01  READ-CNT                  PIC  9(07)     VALUE   ZERO.
 01  WK-RENBAN                 PIC  X(09)     VALUE   SPACE.
*ステータス領域
 01  STATUS-AREA.
     03  EXX-ST                PIC  X(02).
     03  DEN-ST                PIC  X(02).
     03  TBL-ST                PIC  X(02).
     03  SOK-ST                PIC  X(02).
     03  JYO-ST                PIC  X(02).
     03  JYT-ST                PIC  X(02).
*#2020/12/01 NAV ST
     03  KYK-ST                PIC  X(02).
*#2020/12/01 NAV ED
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  EXX-ERR           PIC N(12) VALUE
         NC"小売連携発注データエラー".
     03  DEN-ERR           PIC N(12) VALUE
         NC"売上伝票ファイルエラー".
     03  TBL-ERR           PIC N(12) VALUE
         NC"サブ商品変換ＴＢＬエラー".
     03  SOK-ERR           PIC N(12) VALUE
         NC"倉庫マスタエラー".
     03  JYO-ERR           PIC N(12) VALUE
         NC"条件ファイルエラー".
*#2020/12/01 NAV ST
     03  KYK-ERR           PIC N(15) VALUE
         NC"顧客需要家ＩＤ管理マスタエラー".
*#2020/12/01 NAV ED
     03  JYT-ERR           PIC N(13) VALUE
         NC"Ｄ３６５受注連携ＤＴエラー".
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
 01  MSG-AREA.
     03  MSG-KENSU1.
         05  KENSU1A PIC N(11) VALUE NC"＊＊＊＊＊＊＊＊＊＊＊".
         05  KENSU1B PIC N(11) VALUE NC"＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A  PIC  N(06) VALUE NC"＊　連携ＮＯ".
         05  RENKEI-NO PIC  X(09) VALUE SPACE.
         05  KENSU2B PIC N(11) VALUE NC"　の連携データ　　　＊".
     03  MSG-KENSU3.
         05  KENSU3A  PIC  N(04) VALUE NC"＊　　　".
         05  KENSU3   PIC  Z(05)9 VALUE ZERO.
         05  KENSU3B  PIC  N(15)  VALUE
         NC"件抽出しました　　　　　　　＊".
     03  MSG-KENSU4.
         05  KENSU4A  PIC  N(04) VALUE NC"＊　　　".
         05  KENSU4   PIC  Z(05)9 VALUE ZERO.
         05  KENSU4B  PIC  N(15)  VALUE
         NC"件削除しました　　　　　　　＊".
     03  MSG-KENSU5.
         05  KENSU5A  PIC  N(04) VALUE NC"＊　　　".
         05  KENSU5   PIC  Z(05)9 VALUE ZERO.
         05  KENSU5B  PIC  N(15)  VALUE
         NC"件累積しました　　　　　　　＊".
***** 店舗CD変換
 01  WK-TENPO-CD             PIC   X(06).
 01  WK-TENPO-CD-R           REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-HEN        PIC   9(06).
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
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE              SECTION.
 01   LINK-ORDER-KBN      PIC  X(01).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-ORDER-KBN.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EXXXXXXA.
     MOVE        EXX-ST    TO        E-ST.
     MOVE        "EXXXXXXA" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EXX-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     MOVE        DEN-ST     TO       E-ST.
     MOVE        "SHTDENL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SUBTBLF.
     MOVE        TBL-ST     TO       E-ST.
     MOVE        "SUBTBLL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TBL-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC4              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST     TO       E-ST.
     MOVE        "ZSOKMS1 " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TBL-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC5              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST     TO       E-ST.
     MOVE        "JYOKEN1 " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TBL-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC6              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDNJYTF.
     MOVE        JYT-ST     TO       E-ST.
     MOVE        "SNDNJYTF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*#2020/12/01 NAV ST
 FILEERR-SEC7              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKF.
     MOVE        KYK-ST     TO       E-ST.
     MOVE        "KYKJYKL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KYK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*#2020/12/01 NAV ED
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
     OPEN      INPUT  EXXXXXXA.
     OPEN      OUTPUT SNDNJYTF.
     OPEN      INPUT  SUBTBLF  ZSOKMS  HJYOKEN.
     OPEN      I-O    SHTDENF.
*#2020/12/01 NAV ST
     OPEN      INPUT  KYKJYKF.
*
     PERFORM  EXXXXXXA-READ-SEC.
     IF   END-FLG =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
          MOVE 4000                    TO   PROGRAM-STATUS
          STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    小売連携発注データ読込
****************************************************************
 EXXXXXXA-READ-SEC     SECTION.
     MOVE     "EXXXXXXA-READ-SEC"     TO   S-NAME.
*
     READ  EXXXXXXA AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   EXXXXXXA-READ-EXIT
     END-READ.
*
     ADD   1                 TO   READ-CNT
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "## READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*
 EXXXXXXA-READ-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込　　　
****************************************************************
 SHTDENF-READ-SEC      SECTION.
     MOVE     "SHTDENF-READ-SEC"      TO   S-NAME.
*
     READ  SHTDENF
           INVALID     MOVE  "INV"    TO   SHTDENF-INV-FLG
           NOT INVALID MOVE  SPACE    TO   SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
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
****************************************************************
*    サブ商品名称マスタ読込　　
****************************************************************
 SUBTBLF-READ-SEC      SECTION.
     MOVE     "SUBTBLF-READ-SEC"      TO   S-NAME.
*
     READ  SUBTBLF
           INVALID     MOVE  "INV"    TO   SUBTBLF-INV-FLG
           NOT INVALID MOVE  SPACE    TO   SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
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
*レコード初期化
     MOVE      SPACE         TO   JYT-REC.
     INITIALIZE                   JYT-REC.
*売上伝票索引　　　　　　　　　　　　　　　　　　　　　
*****MOVE      EXX-F063      TO   DEN-F01.
     MOVE      EXX-F05       TO   DEN-F01.
     MOVE      EXX-F13       TO   DEN-F02.
     MOVE      0             TO   DEN-F04.
     MOVE      41            TO   DEN-F051.
     MOVE      EXX-F10       TO   WK-TENPO-CD.
     MOVE      WK-TENPO-HEN  TO   DEN-F07.
     MOVE      EXX-F08       TO   DEN-F112.
     MOVE      EXX-F17       TO   DEN-F03.
     PERFORM  SHTDENF-READ-SEC.
     IF   SHTDENF-INV-FLG = "INV"
          DISPLAY "# " NC"売上伝票Ｆ無異常" " #"  UPON  CONS
          DISPLAY "# F01  = " DEN-F01             UPON  CONS
          DISPLAY "# F02  = " DEN-F02             UPON  CONS
          DISPLAY "# F04  = " DEN-F04             UPON  CONS
          DISPLAY "# F051 = " DEN-F051            UPON  CONS
          DISPLAY "# F07  = " DEN-F07             UPON  CONS
          DISPLAY "# F112 = " DEN-F112            UPON  CONS
          DISPLAY "# F03  = " DEN-F03             UPON  CONS
          MOVE 4000                    TO   PROGRAM-STATUS
          STOP  RUN
     END-IF.
*#2020/12/01 NAV ST
*顧客需要家ＩＤ管理マスタ索引
     MOVE    EXX-F05                   TO   KYK-F01.
     MOVE    WK-TENPO-HEN              TO   KYK-F02.
*****DISPLAY "KYK-F01 = " KYK-F01  " / " KYK-F02  UPON CONS.
     PERFORM KYKJYKF-READ-SEC.
     IF   KYKJYKF-INV-FLG  =  "INV"
          DISPLAY  NC"顧客需要家ＩＤ未登録→"
                   NC"　取引先" " = " DEN-F01
                   NC"　店　舗" " = " DEN-F07
                   NC"　伝票" "NO = " DEN-F02
                   NC"　納品日" " = " DEN-F112
          ADD   1                      TO   ERR-CNT
          GO                           TO   MAIN010
     ELSE *>ＨＤ：顧客ＩＤ、需要家ＩＤセット　ＭＳ：需要家ＩＤ
          MOVE  KYK-F03                TO   JYT-F22(1:10)
          MOVE  KYK-F04                TO   JYT-F24(1:10)
                                            JYT-F61(1:10)
     END-IF.
*#2020/12/01 NAV ED
*サブ商品名称マスタ索引
*****MOVE      EXX-F063      TO   TBL-F01.
     MOVE      EXX-F05       TO   TBL-F01.
     MOVE      EXX-F21       TO   TBL-F02.
*****DISPLAY "TBL = " TBL-F01 " - " TBL-F02  UPON CONS.
     PERFORM   SUBTBLF-READ-SEC.
*****DISPLAY "INV = " SUBTBLF-INV-FLG        UPON CONS.
     IF   SUBTBLF-INV-FLG =  "INV"
          MOVE SPACE         TO   TBL-F17
     END-IF
*****DISPLAY "F17 = " TBL-F17                UPON CONS.
*受注連携データ出力
*    オーダー区分（条件ファイルより取得）　　
     MOVE      27                TO  JYO-F01.
     MOVE      DEN-F32           TO  JYO-F02.
     PERFORM   HJYOKEN-READ-SEC.
     IF   HJYOKEN-INV-FLG  =  "INV"
          DISPLAY  NC"オーダー区分未登録→"
                   NC"　取引先" " = " DEN-F01
                   NC"　店　舗" " = " DEN-F07
                   NC"　伝票" "NO = " DEN-F02
                   NC"　納品日" " = " DEN-F112
                   NC"　オーダー区分" " = " DEN-F32
                   UPON CONS
                   MOVE 4000          TO    PROGRAM-STATUS
                   STOP  RUN
     ELSE  *>マスタ内のオーダー区分、直送ＦＬＧをセットする。
          MOVE JYO-F14(1:3)  TO   JYT-F04(1:3)
          MOVE JYO-F15(1:1)  TO   JYT-F51
     END-IF.
*#2021/05/14 NAV ST 単区＝５，６の時の対応
*　　見本鉢オーダー区分対応　条件Ｆの予備（１：３）セット
     IF   DEN-F16  =  "5"
          IF  JYO-FIL1(1:3)  NOT =  SPACE
              MOVE JYO-FIL1(1:3)  TO   JYT-F04(1:3)
          END-IF
     END-IF.
*　　見本鉢オーダー区分対応　条件Ｆの予備（４：３）セット
     IF   DEN-F16  =  "6"
          IF  JYO-FIL1(4:3)  NOT =  SPACE
              MOVE JYO-FIL1(4:3)  TO   JYT-F04(1:3)
          END-IF
     END-IF.
*#2021/05/14 NAV ED 単区＝５，６の時の対応
*    Ｄ３６５伝票番号
*****MOVE      DEN-D99       TO   JYT-F01.
     MOVE      DEN-D99(1:15) TO   JYT-F01(1:15).
     MOVE      JYT-F04(1:3)  TO   JYT-F01(16:3).
*# 2020/07/20 NAV ED
*    受注日
*****MOVE      EXX-F07(1:4)  TO   JYT-F05(1:4).
*    MOVE      "/"           TO   JYT-F05(5:1).
*    MOVE      EXX-F07(5:2)  TO   JYT-F05(6:2).
*    MOVE      "/"           TO   JYT-F05(8:1).
*****MOVE      EXX-F07(7:2)  TO   JYT-F05(9:2).
*#2021/06/24 NAV ST 日付セット仕様変更
*****MOVE      EXX-F08(1:4)  TO   JYT-F05(1:4).
*    MOVE      "/"           TO   JYT-F05(5:1).
*    MOVE      EXX-F08(5:2)  TO   JYT-F05(6:2).
*    MOVE      "/"           TO   JYT-F05(8:1).
*****MOVE      EXX-F08(7:2)  TO   JYT-F05(9:2).
*#2021/06/24 NAV ED
*#2022/02/28 NAV ST 実納品日セットに変更　　
     MOVE      EXX-F09(1:4)  TO   JYT-F05(1:4).
     MOVE      "/"           TO   JYT-F05(5:1).
     MOVE      EXX-F09(5:2)  TO   JYT-F05(6:2).
     MOVE      "/"           TO   JYT-F05(8:1).
     MOVE      EXX-F09(7:2)  TO   JYT-F05(9:2).
*#2021/06/24 NAV ED
*    販売価格設定
     MOVE      "1"           TO   JYT-F10.
*    倉庫　　　　　　　
     MOVE      EXX-F18       TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "SSC"         TO   JYT-F11
          MOVE "0"           TO   JYT-F12(1:1)
          MOVE  EXX-F18      TO   JYT-F12(2:2)
          MOVE  "11"         TO   JYT-F13
     ELSE
**********サイト取得
          MOVE  41           TO   JYO-F01
          MOVE  SOK-F15      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "SSC"    TO   JYT-F11
          ELSE
              MOVE   JYO-F14 TO   JYT-F11
          END-IF
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16      TO   JYT-F12
**********保管場所取得
          MOVE  42           TO   JYO-F01
          MOVE  SOK-F17      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "11"     TO   JYT-F13
          ELSE
              MOVE   JYO-F14 TO   JYT-F13
          END-IF
     END-IF.
*#2020/12/14 NAV ST 下記のオーダー区分の場合は、
*#                  サイト、倉庫、保管場所＝空白をセットする。
*#2021/07/06 NAV ST Ｐ３１，Ｐ４１追加
*****IF   JYT-F04(1:3)  =  "D21"  OR  "D31"
     IF   JYT-F04(1:3)  =  "D21" OR "D31" OR "P31" OR "P41"
          MOVE   SPACE  TO        JYT-F11  JYT-F12  JYT-F13
     END-IF.
*#2021/07/06 NAV ED Ｐ３１，Ｐ４１追加
*#2020/12/14 NAV ED
*    出荷予定日
     MOVE    "6"        TO        LINK-IN-KBN.
     MOVE     1         TO        LINK-IN-YMD6.
     MOVE     EXX-F08   TO        LINK-IN-YMD8.
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
*****MOVE      LINK-OUT-YMD8(1:4)  TO   JYT-F14(1:4).
*    MOVE      "/"           TO   JYT-F14(5:1).
*    MOVE      LINK-OUT-YMD8(5:2)  TO   JYT-F14(6:2).
*    MOVE      "/"           TO   JYT-F14(8:1).
*****MOVE      LINK-OUT-YMD8(7:2)  TO   JYT-F14(9:2).
*#2021/06/24 NAV ST 日付セット仕様変更
*****MOVE      EXX-F08(1:4)  TO   JYT-F14(1:4).
*    MOVE      "/"           TO   JYT-F14(5:1).
*    MOVE      EXX-F08(5:2)  TO   JYT-F14(6:2).
*    MOVE      "/"           TO   JYT-F14(8:1).
*****MOVE      EXX-F08(7:2)  TO   JYT-F14(9:2).
*#2021/06/24 NAV ED
*#2022/02/28 NAV ST 実納品日セットに変更
     MOVE      EXX-F09(1:4)  TO   JYT-F14(1:4).
     MOVE      "/"           TO   JYT-F14(5:1).
     MOVE      EXX-F09(5:2)  TO   JYT-F14(6:2).
     MOVE      "/"           TO   JYT-F14(8:1).
     MOVE      EXX-F09(7:2)  TO   JYT-F14(9:2).
*#2021/06/24 NAV ED
*    入荷希望日
*#2022/02/28 NAV ST 実納品日セットに変更
*****MOVE      EXX-F08(1:4)  TO   JYT-F15(1:4).
*    MOVE      "/"           TO   JYT-F15(5:1).
*    MOVE      EXX-F08(5:2)  TO   JYT-F15(6:2).
*    MOVE      "/"           TO   JYT-F15(8:1).
*****MOVE      EXX-F08(7:2)  TO   JYT-F15(9:2).
     MOVE      EXX-F09(1:4)  TO   JYT-F15(1:4).
     MOVE      "/"           TO   JYT-F15(5:1).
     MOVE      EXX-F09(5:2)  TO   JYT-F15(6:2).
     MOVE      "/"           TO   JYT-F15(8:1).
     MOVE      EXX-F09(7:2)  TO   JYT-F15(9:2).
*#2022/02/28 NAV ED
*    顧客発注番号
     MOVE      EXX-F13       TO   JYT-F18.
*    外部システム番号
*# 2020/07/20 NAV ST
*****MOVE      DEN-D99       TO   JYT-F19.
     MOVE      JYT-F01       TO   JYT-F19.
*# 2020/07/20 NAV ED
*    連携ＮＯ
     MOVE      EXX-F01       TO   JYT-F20.
*    連携データ区分
     MOVE      "1"           TO   JYT-F21.
*    旧顧客ＣＤ番号
     MOVE      EXX-F04       TO   JYT-F23.
*    旧納入先ＣＤ
     MOVE      EXX-F10       TO   WK-TENPO-CD.
     MOVE      WK-TENPO-HEN  TO   JYT-F25.
*    一時出荷先フラグ
     MOVE      "0"           TO   JYT-F26.
*    旧商品ＣＤ
*    ＪＡＮＣＤ
     MOVE      TBL-F17       TO   JYT-F46.
*    数量符号
     MOVE      "-"           TO   JYT-F470.
*    数量　　　　　
     MOVE      EXX-F22       TO   JYT-F47.
     COMPUTE JYT-F47 = EXX-F22 * 100.
*    直送フラグ（先行受注分ははすべて直送扱いとする）
*#2020/12/15 NAV ST
*TTTTMOVE      "1"           TO   JYT-F51.
*#2020/12/15 NAV ED
*****EVALUATE  LINK-ORDER-KBN
*TTTTEVALUATE  DEN-F32
*       WHEN  "1"  MOVE "0"      TO  JYT-F51
*       WHEN  "2"  MOVE "1"      TO  JYT-F51
*       WHEN  "3"  MOVE "0"      TO  JYT-F51
*TTTTEND-EVALUATE.
*    販売価格設定
     MOVE      "1"           TO   JYT-F57.
*    販売単価
     MOVE      EXX-F23       TO   JYT-F58.
     COMPUTE JYT-F58 = EXX-F23 * 100.
*    受注金額符号
     MOVE      "-"           TO   JYT-F590.
*    受注金額
     MOVE      EXX-F25       TO   JYT-F59.
     COMPUTE JYT-F59 = EXX-F25 * 100.
*    販売価格単位
     MOVE      "1"           TO   JYT-F60.
*    旧納入先ＣＤ
     MOVE      WK-TENPO-HEN  TO   JYT-F62.
*    出荷先フラグ
     MOVE      "0"           TO   JYT-F63.
*2021/04/30 NAV ST 明細備考をセット
     MOVE      EXX-F27       TO   JYT-F74.
*2021/04/30 NAV ED 明細備考をセット
*    外部システム番号
*# 2020/07/20 NAV ST
*****MOVE      DEN-D99       TO   JYT-F77.
     MOVE      JYT-F01       TO   JYT-F77.
*# 2020/07/20 NAV ED
*    外部システム行番号
     MOVE      EXX-F17       TO   JYT-F78.
*    売上伝票ファイル項目セット
     MOVE      "1"           TO   DEN-D95.
     MOVE      JYT-F04       TO   DEN-D96.
*#2021/05/14 NAV ST サービス対応
     IF   JYT-F04(1:3)  =  "D21"  OR  "D31"   OR  "P31"  OR
                           "P41"
          MOVE   "1"    TO        JYT-F09
     END-IF.
*#2021/05/14 NAV ED サービス対応
*## 2020/06/03 NAV ST
     MOVE      "1"           TO   DEN-D93.
*## 2020/06/03 NAV ED
     REWRITE   DEN-REC.
*
     WRITE  JYT-REC.
     ADD       1             TO   WRT-CNT.
 MAIN010.
*
     PERFORM  EXXXXXXA-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     EXXXXXXA.
     CLOSE     SNDNJYTF.
     CLOSE     SHTDENF  SUBTBLF  ZSOKMS  HJYOKEN.
*#2020/12/01 NAV ST
     CLOSE     KYKJYKF.
*#2020/12/01 NAV ED
*
     DISPLAY "## READ-CNT = " READ-CNT   " ##" UPON CONS.
     DISPLAY "## WRT-CNT  = " WRT-CNT    " ##" UPON CONS.
*
     DISPLAY "## ERR-CNT  = " ERR-CNT    " ##" UPON CONS.
*
     IF  ERR-CNT  >  ZERO
         DISPLAY NC"！！顧客需要家ＩＤ管理マスタに未登録の"
                 NC"取引先、店舗が存在します。マスタ登録し"
                 NC"管理番号で取消後、再実行して下さい！！"
         MOVE    4010                      TO  PROGRAM-STATUS
     END-IF.
*
*
 END-EXIT.
     EXIT.

```
