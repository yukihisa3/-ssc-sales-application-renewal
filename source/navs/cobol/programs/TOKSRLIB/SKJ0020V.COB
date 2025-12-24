****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　        *
*    モジュール名　　　　：　顧客需要家ＩＤデータＣＳＶ出力　  *
*    作成日／更新日　　　：　20/11/19                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　顧客需要家ＩＤ取込ＤＴを読み、　　*
*    　　　　　　　　　　　　顧客需要家ＩＤ取込ＤＴをＣＳＶに　*
*    　　　　　　　　　　　　出力擦る。　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　            *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKJ0020V.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/11/19.
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
*顧客需要家ＩＤ取込データ
     SELECT  KYKJYKWK  ASSIGN    TO        KYKJYKW1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KYK-F01
                                           KYK-F02
                       FILE      STATUS    KYK-ST.
*
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
*顧客需要家ＩＤ取込ＤＴＣＳＶ
     SELECT  KYKJYKCV  ASSIGN    TO        KYKJYKCV
                       FILE      STATUS    CSV-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 顧客需要家ＩＤ取込データ                           *
****************************************************************
 FD  KYKJYKWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KYKJYKWK  OF   XFDLIB
                       JOINING   KYK       AS   PREFIX.
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
*    FILE = ストックＮＯ実績データＣＳＶ                       *
****************************************************************
 FD  KYKJYKCV          BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER        PIC  X(1500).
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KYK-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  CSV-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  TAISYO-FLG               PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
*カウンタ領域
 01  COUNTER.
     03  READ-CNT                 PIC  9(08)  VALUE  ZERO.
     03  CSV-CNT                  PIC  9(08)  VALUE  ZERO.
     03  TAISYO-CNT               PIC  9(06)  VALUE  ZERO.
     03  WK-GK-SYUKASU            PIC  9(06)  VALUE  ZERO.
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
     03  KYK-ERR           PIC N(20) VALUE
                        NC"顧客需要家ＩＤ取込ＤＴエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  CSV-ERR           PIC N(20) VALUE
         NC"顧客需要家管理データＣＳＶエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(14)  VALUE
                       NC"＜顧客需要家データ取込結果＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"システム日付：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H01           PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"システム時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H02           PIC X(08).
     03  FILLER        PIC X(03)  VALUE  ",,,".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"エラー内容＝＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"取引先Ｍ未登録".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"店舗Ｍ未登録".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"顧客ＩＤ未登録".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"需要家ＩＤ未登録".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"削除区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"＊＊＊＊＊＊".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"＊＊＊＊＊＊".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"＊＊＊＊＊＊".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"＊＊＊＊＊＊".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"＊＊＊＊＊＊".
     03  FILLER        PIC X(01)  VALUE  X"29".
 01  WK-HEAD2.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"ＮＡＶＳ取引先ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"ＮＡＶＳ取引先名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"ＮＡＶＳ店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ＮＡＶＳ店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"Ｄ３６５顧客ＩＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"Ｄ３６５需要家ＩＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"削除区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"エラー区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容３".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容４".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容５".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容６".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容７".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容８".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"エラー内容９".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"エラー内容１０".
     03  FILLER        PIC X(01)  VALUE  X"29".
*明細エリア
 01  WK-MEISAI1.
     03  M01           PIC 9(08).
     03  K01           PIC X(01).
     03  M02S          PIC X(01).
     03  M02           PIC N(20).
     03  M02E          PIC X(01).
     03  K02           PIC X(01).
     03  M03           PIC 9(05).
     03  K03           PIC X(01).
     03  M04S          PIC X(01).
     03  M04           PIC N(20).
     03  M04E          PIC X(01).
     03  K04           PIC X(01).
     03  M05           PIC X(10).
     03  K05           PIC X(01).
     03  M06           PIC X(10).
     03  K06           PIC X(01).
     03  M07           PIC X(01).
     03  K07           PIC X(01).
     03  M08S          PIC X(01).
     03  M08           PIC N(01).
     03  M08E          PIC X(01).
     03  K08           PIC X(01).
     03  M09S          PIC X(01).
     03  M09           PIC N(01).
     03  M09E          PIC X(01).
     03  K09           PIC X(01).
     03  M10S          PIC X(01).
     03  M10           PIC N(01).
     03  M10E          PIC X(01).
     03  K10           PIC X(01).
     03  M11S          PIC X(01).
     03  M11           PIC N(01).
     03  M11E          PIC X(01).
     03  K11           PIC X(01).
     03  M12S          PIC X(01).
     03  M12           PIC N(01).
     03  M12E          PIC X(01).
     03  K12           PIC X(01).
     03  M13S          PIC X(01).
     03  M13           PIC N(01).
     03  M13E          PIC X(01).
     03  K13           PIC X(01).
     03  M14S          PIC X(01).
     03  M14           PIC N(01).
     03  M14E          PIC X(01).
     03  K14           PIC X(01).
     03  M15S          PIC X(01).
     03  M15           PIC N(01).
     03  M15E          PIC X(01).
     03  K15           PIC X(01).
     03  M16S          PIC X(01).
     03  M16           PIC N(01).
     03  M16E          PIC X(01).
     03  K16           PIC X(01).
     03  M17S          PIC X(01).
     03  M17           PIC N(01).
     03  M17E          PIC X(01).
     03  K17           PIC X(01).
     03  M18S          PIC X(01).
     03  M18           PIC N(01).
     03  M18E          PIC X(01).
     03  K18           PIC X(01).
 LINKAGE                   SECTION.
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKWK.
     DISPLAY     KYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KYK-ST    UPON      CONS.
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
 CSV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KYKJYKCV.
     DISPLAY     CSV-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CSV-ST    UPON      CONS.
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
     OPEN     INPUT  KYKJYKWK  HTOKMS  HTENMS.
     OPEN     OUTPUT KYKJYKCV.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*カウンタの初期化
     INITIALIZE         COUNTER.
*顧客需要家ＩＤ取込データ
     PERFORM  KYKJYKWK-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃対象データ無！＃" UPON CONS
          MOVE  4010     TO   PROGRAM-STATUS
          GO             TO   INIT-EXIT
     END-IF.
*タイトル行出力
     MOVE SPACE          TO   CSV-REC.
     MOVE HEN-DATE       TO   H01.
     MOVE HEN-TIME       TO   H02.
     MOVE WK-HEAD1       TO   CSV-REC.
     WRITE CSV-REC.
     ADD   1             TO   CSV-CNT.
     MOVE SPACE          TO   CSV-REC.
     MOVE WK-HEAD2       TO   CSV-REC.
     WRITE CSV-REC.
     ADD   1             TO   CSV-CNT.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     MOVE      SPACE         TO   CSV-REC  WK-MEISAI1.
*    ＣＳＶ（カンマ）セット
     MOVE      ","           TO   K01 K02 K03 K04 K05 K06 K07 K08
         K09 K10 K11 K12 K13 K14 K15 K16 K17.
*    制御バイトセット
     MOVE X"28"              TO   M02S M04S M08S M09S M10S
         M11S M12S M13S M14S M15S M16S M17S M18S.
     MOVE X"29"              TO   M02E M04E M08E M09E M10E
         M11E M12E M13E M14E M15E M16E M17E M18E.
*    ＮＡＶＳ取引先ＣＤ
     MOVE  KYK-F01           TO   M01.
*    ＮＡＶＳ取引先名
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M02
     ELSE
         MOVE TOK-F02        TO   M02
     END-IF.
*    ＮＡＶＳ店舗ＣＤ　
     MOVE  KYK-F02           TO   M03.
*    ＮＡＶＳ店舗名
     PERFORM  HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M04
     ELSE
         MOVE TEN-F02        TO   M04
     END-IF.
*    Ｄ３６５顧客ＩＤ
     MOVE  KYK-F03           TO   M05.
*    Ｄ３６５需要家ＩＤ
     MOVE  KYK-F04           TO   M06.
*    削除区分
     MOVE  KYK-F05           TO   M07.
*    エラー区分　
     IF    KYK-F06  =  "1"
         MOVE NC"〇"         TO   M08
     END-IF.
*    エラー内容１
     IF    KYK-F070  =  "1"
         MOVE NC"〇"         TO   M09
     END-IF.
*    エラー内容２
     IF    KYK-F071  =  "1"
         MOVE NC"〇"         TO   M10
     END-IF.
*    エラー内容３
     IF    KYK-F072  =  "1"
         MOVE NC"〇"         TO   M11
     END-IF.
*    エラー内容４
     IF    KYK-F073  =  "1"
         MOVE NC"〇"         TO   M12
     END-IF.
*    エラー内容５
     IF    KYK-F074  =  "1"
         MOVE NC"〇"         TO   M13
     END-IF.
*    エラー内容６
     IF    KYK-F075  =  "1"
         MOVE NC"〇"         TO   M14
     END-IF.
*    エラー内容７
     IF    KYK-F076  =  "1"
         MOVE NC"〇"         TO   M15
     END-IF.
*    エラー内容８
     IF    KYK-F077  =  "1"
         MOVE NC"〇"         TO   M16
     END-IF.
*    エラー内容９
     IF    KYK-F078  =  "1"
         MOVE NC"〇"         TO   M17
     END-IF.
*    エラー内容１０
     IF    KYK-F079  =  "1"
         MOVE NC"〇"         TO   M18
     END-IF.
*
     MOVE WK-MEISAI1         TO   CSV-REC.
     WRITE CSV-REC.
     ADD   1                 TO   CSV-CNT.
*ストックＮＯ管理ファイル読込
     PERFORM  KYKJYKWK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             KYKJYKWK  HTOKMS  HTENMS  KYKJYKCV.
**
     DISPLAY "# READ-CNT = " READ-CNT    UPON  CONS.
     DISPLAY "# CSV-CNT    " CSV-CNT     UPON  CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    顧客需要家ＩＤ取込データ読込　　　　　　　　　　　　      *
****************************************************************
 KYKJYKWK-READ-SEC     SECTION.
*
     MOVE "KYKJYKWK-READ-SEC"     TO   S-NAME.
     READ    KYKJYKWK
             AT  END
             MOVE  "END"     TO   END-FLG
             GO              TO   KYKJYKWK-READ-EXIT
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
 KYKJYKWK-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE  KYK-F01                TO   TOK-F01.
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
     MOVE  KYK-F01                TO   TEN-F52.
     MOVE  KYK-F02                TO   TEN-F011.
     READ  HTENMS
           INVALID        MOVE "INV"   TO  HTENMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
*****************<<  SKJ0020V   END PROGRAM  >>******************
