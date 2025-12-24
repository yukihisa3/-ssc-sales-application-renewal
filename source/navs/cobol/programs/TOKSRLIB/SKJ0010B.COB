****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携マスタ管理　　　      *
*    モジュール名　　　　：　顧客需要家ＩＤデータ取込＆チェック*
*    作成日／更新日　　　：　20/11/24                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　顧客需要家ＩＤ取込ＤＴを読み、　　*
*    　　　　　　　　　　　　顧客需要家ＩＤ管理マスタへの更新を*
*    　　　　　　　　　　　　行なう。エラーチェック実施　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　            *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKJ0010B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/11/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ＥＸＣＥＬ顧客需要家データ　
     SELECT  ELKJDATA  ASSIGN    TO        ELKJDATA
                       FILE      STATUS    CSV-ST.
*顧客需要家ＩＤ取込データ
     SELECT  KYKJYKWK  ASSIGN    TO        KYKJYKW1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KYK-F01
                                           KYK-F02
                       FILE      STATUS    KYK-ST.
*
*顧客需要家ＩＤ管理マスタ　　
     SELECT  KYKJYKF   ASSIGN    TO        KYKJYKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KKJ-F01
                                           KKJ-F02
                       FILE      STATUS    KKJ-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52  TEN-F011
                       FILE      STATUS    TEN-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ＥＸＣＥＬ顧客需要家データ　                       *
****************************************************************
 FD  ELKJDATA
                        LABEL    RECORD    IS   STANDARD
                        BLOCK    CONTAINS  120  RECORDS.
                        COPY     ELKJDATA  OF   XFDLIB
                        JOINING  CSV       AS   PREFIX.
****************************************************************
*    FILE = 顧客需要家ＩＤ取込データ                           *
****************************************************************
 FD  KYKJYKWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KYKJYKWK  OF   XFDLIB
                       JOINING   KYK       AS   PREFIX.
****************************************************************
*    FILE = 顧客需要家ＩＤ管理マスタ　                         *
****************************************************************
 FD  KYKJYKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KYKJYKF   OF   XFDLIB
                       JOINING   KKJ       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ　                                       *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  CSV-ST                   PIC  X(02).
     03  KYK-ST                   PIC  X(02).
     03  KKJ-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  TAISYO-FLG               PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  KYKJYKWK-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  KYKJYKF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  ERR-KBN                  PIC  X(01)  VALUE  SPACE.
*カウンタ領域
 01  COUNTER.
     03  READ-CNT                 PIC  9(08)  VALUE  ZERO.
     03  CSV-CNT                  PIC  9(08)  VALUE  ZERO.
     03  TAISYO-CNT               PIC  9(06)  VALUE  ZERO.
     03  WK-GK-SYUKASU            PIC  9(06)  VALUE  ZERO.
     03  KYK-REW-CNT              PIC  9(06)  VALUE  ZERO.
     03  KYK-WRT-CNT              PIC  9(06)  VALUE  ZERO.
     03  KKJ-REW-CNT              PIC  9(06)  VALUE  ZERO.
     03  KKJ-WRT-CNT              PIC  9(06)  VALUE  ZERO.
     03  KKJ-DEL-CNT              PIC  9(06)  VALUE  ZERO.
     03  KYK-ERR-CNT              PIC  9(06)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  インデックス
     03  IXA                      PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*発注日、納品日編集
 01  WK-HENKAN-DATE               PIC   9(08)  VALUE  ZERO.
 01  WK-OUT-DATE.
     03  WK-OUT-YYYY              PIC   9(04)  VALUE  ZERO.
     03  WK-OUT-MM                PIC   9(02)  VALUE  ZERO.
     03  WK-OUT-DD                PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  CSV-ERR           PIC N(20) VALUE
                        NC"ＥＸＣＥＬ顧客需要家データエラー".
     03  KYK-ERR           PIC N(20) VALUE
                        NC"顧客需要家ＩＤ取込ＤＴエラー".
     03  KKJ-ERR           PIC N(20) VALUE
                        NC"顧客需要家ＩＤ管理マスタエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  LINK-BUMON            PIC  X(04).
 01  LINK-TANCD            PIC  X(02).
 01  LINK-ERRKBN           PIC  X(01).
**************************************************************
 PROCEDURE             DIVISION
          USING  LINK-BUMON  LINK-TANCD  LINK-ERRKBN.
**************************************************************
 DECLARATIVES.
 CSV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ELKJDATA.
     DISPLAY     CSV-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CSV-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKWK.
     DISPLAY     KYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KYK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KKJ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKF.
     DISPLAY     KKJ-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KKJ-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     INPUT  ELKJDATA  HTOKMS  HTENMS.
     OPEN     I-O    KYKJYKWK  KYKJYKF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*カウンタの初期化
     INITIALIZE         COUNTER.
*顧客需要家ＩＤ取込データ
     PERFORM  ELKJDATA-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃対象データ無！＃" UPON CONS
          MOVE  4010     TO   PROGRAM-STATUS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*顧客需要家ＩＤ取込データ存在チェック
     PERFORM KYKJYKWK-READ-SEC.
*顧客需要家ＩＤ管理マスタ存在チェック
     PERFORM KYKJYKF-READ-SEC.
 MAIN010.
*ＥＸＣＥＬ顧客需要家データセット
     IF  KYKJYKWK-INV-FLG = "INV"
         MOVE    SPACE       TO   KYK-REC
         INITIALIZE               KYK-REC
*********取引先ＣＤ　　　　
         MOVE    CSV-F01     TO   KYK-F01
*********店舗ＣＤ　　　　
         MOVE    CSV-F02     TO   KYK-F02
     END-IF.
 MAIN020.
*顧客ＩＤ、需要家ＩＤ、削除区分セット
     MOVE  CSV-F03           TO   KYK-F03.
     MOVE  CSV-F04           TO   KYK-F04.
     MOVE  CSV-F05           TO   KYK-F05.
*エラーチェック
     MOVE  SPACE             TO   ERR-KBN.
 MAIN030.
*    取引先マスタ存在チェック
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE "1"            TO   KYK-F070  ERR-KBN
     ELSE
         MOVE SPACE          TO   KYK-F070
     END-IF.
 MAIN040.
*    店舗マスタ存在チェック
     PERFORM  HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE "1"            TO   KYK-F071  ERR-KBN
     ELSE
         MOVE SPACE          TO   KYK-F071
     END-IF.
*    顧客ＩＤチェック
     IF  CSV-F03(1:1)  =  SPACE
     OR  CSV-F03(2:1)  =  SPACE
     OR  CSV-F03(3:1)  =  SPACE
     OR  CSV-F03(4:1)  =  SPACE
     OR  CSV-F03(5:1)  =  SPACE
     OR  CSV-F03(6:1)  =  SPACE
     OR  CSV-F03(7:1)  =  SPACE
     OR  CSV-F03(8:1)  =  SPACE
         MOVE "1"            TO   KYK-F072  ERR-KBN
     ELSE
         MOVE SPACE          TO   KYK-F072
     END-IF.
*    顧客ＩＤチェック
     IF  CSV-F04(1:1)  =  SPACE
     OR  CSV-F04(2:1)  =  SPACE
     OR  CSV-F04(3:1)  =  SPACE
     OR  CSV-F04(4:1)  =  SPACE
     OR  CSV-F04(5:1)  =  SPACE
     OR  CSV-F04(6:1)  =  SPACE
     OR  CSV-F04(7:1)  =  SPACE
     OR  CSV-F04(8:1)  =  SPACE
         MOVE "1"            TO   KYK-F073  ERR-KBN
     ELSE
         MOVE SPACE          TO   KYK-F073
     END-IF.
*    削除区分　　　　
     IF  CSV-F05  =  SPACE  OR  "1"
         MOVE SPACE          TO   KYK-F074
     ELSE
         MOVE "1"            TO   KYK-F074  ERR-KBN
     END-IF.
*    エラー判定
     IF  ERR-KBN  =  "1"
         MOVE "1"            TO   KYK-F06
         ADD   1             TO   KYK-ERR-CNT
     END-IF.
 MAIN050.
*顧客需要家ＩＤ取込データ更新チェック
     IF  KYKJYKWK-INV-FLG = "INV"
*********登録
         MOVE  LINK-BUMON    TO   KYK-F92
         MOVE  LINK-TANCD    TO   KYK-F93
         MOVE  SYS-DATE      TO   KYK-F94
         MOVE  WK-TIME(1:6)  TO   KYK-F95
         WRITE  KYK-REC
         ADD   1             TO   KYK-WRT-CNT
     ELSE
         MOVE  "1"           TO   KYK-F079
         MOVE  LINK-BUMON    TO   KYK-F96
         MOVE  LINK-TANCD    TO   KYK-F97
         MOVE  SYS-DATE      TO   KYK-F98
         MOVE  WK-TIME(1:6)  TO   KYK-F99
         REWRITE  KYK-REC
         ADD   1             TO   KYK-REW-CNT
     END-IF.
 MAIN080.
*顧客需要家ＩＤ管理マスタ更新
     IF  ERR-KBN  =  SPACE
        IF  KYKJYKF-INV-FLG =  "INV"
            MOVE SPACE          TO   KKJ-REC
            INITIALIZE               KKJ-REC
            MOVE  KYK-F01       TO   KKJ-F01
            MOVE  KYK-F02       TO   KKJ-F02
            MOVE  KYK-F03       TO   KKJ-F03
            MOVE  KYK-F04       TO   KKJ-F04
            MOVE  LINK-BUMON    TO   KKJ-F92
            MOVE  LINK-TANCD    TO   KKJ-F93
            MOVE  SYS-DATE      TO   KKJ-F94
            MOVE  WK-TIME(1:6)  TO   KKJ-F95
            IF  CSV-F05  NOT =  "1"
                WRITE KKJ-REC
                ADD   1             TO   KKJ-WRT-CNT
            END-IF
        ELSE
            IF   CSV-F05  NOT =  "1"
                 MOVE  KYK-F03       TO   KKJ-F03
                 MOVE  KYK-F04       TO   KKJ-F04
                 MOVE  LINK-BUMON    TO   KKJ-F96
                 MOVE  LINK-TANCD    TO   KKJ-F97
                 MOVE  SYS-DATE      TO   KKJ-F98
                 MOVE  WK-TIME(1:6)  TO   KKJ-F99
                 REWRITE  KKJ-REC
                 ADD   1             TO   KKJ-REW-CNT
            ELSE
                 DELETE  KYKJYKF
                 ADD   1             TO   KKJ-DEL-CNT
            END-IF
        END-IF
     END-IF.
*顧客需要家ＩＤ取込データ
     PERFORM  ELKJDATA-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  KYKJYKWK  HTOKMS  HTENMS  ELKJDATA  KYKJYKF.
*
     IF  KYK-ERR-CNT  >  ZERO
         MOVE   "1"           TO    LINK-ERRKBN
     END-IF.
**
     DISPLAY "# READ-CNT = " READ-CNT            UPON  CONS.
     DISPLAY "# ERR-CNT  = " KYK-ERR-CNT         UPON  CONS.
     DISPLAY "# KYK-RW-CNT = " KYK-REW-CNT         UPON  CONS.
     DISPLAY "# KYK-WT-CNT = " KYK-WRT-CNT         UPON  CONS.
     DISPLAY "# KYJ-RE-CNT = " KKJ-REW-CNT         UPON  CONS.
     DISPLAY "# KYJ-WT-CNT = " KKJ-WRT-CNT         UPON  CONS.
     DISPLAY "# KYJ-DL-CNT = " KKJ-DEL-CNT         UPON  CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    顧客需要家ＩＤ取込データ読込　　　　　　　　　　　　      *
****************************************************************
 ELKJDATA-READ-SEC     SECTION.
*
     MOVE "ELKJDATA-READ-SEC"     TO   S-NAME.
     READ    ELKJDATA
             AT  END
             MOVE  "END"     TO   END-FLG
             GO              TO   ELKJDATA-READ-EXIT
             NOT  AT  END
             ADD   1         TO   READ-CNT
     END-READ.
*件数表示
     IF  READ-CNT(6:3) =  "000" OR  "500"
         DISPLAY "READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*
     ADD     1               TO   TAISYO-CNT.
*
 ELKJDATA-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE  CSV-F01                TO   TOK-F01.
     READ  HTOKMS
           INVALID        MOVE "INV"   TO  HTOKMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ読込　　　　　　　　　　                        *
****************************************************************
 HTENMS-READ-SEC        SECTION.
     MOVE     "HTENMS-READ-SEC"   TO   S-NAME.
*
     MOVE  CSV-F01                TO   TEN-F52.
     MOVE  CSV-F02                TO   TEN-F011.
     READ  HTENMS
           INVALID        MOVE "INV"   TO  HTENMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    顧客需要家ＩＤ取込データ　　　　　                        *
****************************************************************
 KYKJYKWK-READ-SEC      SECTION.
     MOVE     "KYKJYKWK-READ-SEC" TO   S-NAME.
*
     MOVE  CSV-F01                TO   KYK-F01.
     MOVE  CSV-F02                TO   KYK-F02.
     READ  KYKJYKWK
           INVALID        MOVE "INV"   TO  KYKJYKWK-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  KYKJYKWK-INV-FLG
     END-READ.
*
 KYKJYKWK-READ-EXIT.
     EXIT.
****************************************************************
*    顧客需要家ＩＤ管理マスタ　　　　　                        *
****************************************************************
 KYKJYKF-READ-SEC       SECTION.
     MOVE     "KYKJYKF-READ-SEC"  TO   S-NAME.
*
     MOVE  CSV-F01                TO   KKJ-F01.
     MOVE  CSV-F02                TO   KKJ-F02.
     READ  KYKJYKF
           INVALID        MOVE "INV"   TO  KYKJYKF-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  KYKJYKF-INV-FLG
     END-READ.
*
 KYKJYKF-READ-EXIT.
     EXIT.
*****************<<  SKJ0010B   END PROGRAM  >>******************
