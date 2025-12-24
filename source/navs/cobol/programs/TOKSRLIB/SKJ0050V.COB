****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　        *
*    モジュール名　　　　：　顧客需要家ＩＤデータマスタ出力１  *
*    作成日／更新日　　　：　20/11/20                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタで受け取った取引先範囲、　*
*    　　　　　　　　　　　　で顧客需要家ＩＤ管理Ｍを読み範囲　*
*    　　　　　　　　　　　　内のマスタリストを発行する。　　　*
*    　　　　　　　　　　　　（初期データ）　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　            *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKJ0050V.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/11/20.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*
*店舗マスタ（ＳＥＱ）
     SELECT  TENMS1    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       TEN-F52  TEN-F011
                       FILE      STATUS    TEN-ST.
*
*顧客需要家ＩＤ取込ＤＴＣＳＶ
     SELECT  KYKJYKCV  ASSIGN    TO        KYKJYKCV
                       FILE      STATUS    CSV-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 取引先マスタ                                      *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ（ＲＡＮＤＯＭ）　                       *
****************************************************************
 FD  TENMS1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      TENMS1    OF   XFDLIB
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
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  CSV-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  TAISYO-FLG               PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  TENMS1-INV-FLG           PIC  X(03)  VALUE  SPACE.
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
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタ（Ｒ）エラー".
     03  CSV-ERR           PIC N(20) VALUE
         NC"顧客需要家管理データＣＳＶエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*データ出力ワーク
 01  WK-MEISAI2.
     03  MM1           PIC 9(08).
     03  MK1           PIC X(01).
     03  MM2           PIC 9(05).
     03  MK2           PIC X(01).
     03  MM3           PIC X(10).
     03  MK3           PIC X(01).
     03  MM4           PIC X(10).
     03  MK4           PIC X(01).
     03  MM5S          PIC X(01).
     03  MM5           PIC N(20).
     03  MM5E          PIC X(01).
     03  MK5           PIC X(01).
     03  MM6S          PIC X(01).
     03  MM6           PIC N(20).
     03  MM6E          PIC X(01).
 LINKAGE                   SECTION.
 01  PARA-TOKCDS       PIC 9(08).
 01  PARA-TOKCDE       PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION
     USING  PARA-TOKCDS PARA-TOKCDE.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TENMS1.
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
     OPEN     INPUT  TENMS1  HTOKMS.
     OPEN     OUTPUT KYKJYKCV.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*カウンタの初期化
     INITIALIZE         COUNTER.
*出力判定
*********店舗マスタスタート
     MOVE   SPACE     TO     TEN-REC
     INITIALIZE              TEN-REC
     MOVE   PARA-TOKCDS  TO  TEN-F52.
     START  TENMS1   KEY  IS  >=  TEN-F52  TEN-F011
            INVALID
            MOVE     "END"    TO   END-FLG
            DISPLAY  NC"＃対象ＤＴ無！_" UPON CONS
            MOVE     4010     TO   PROGRAM-STATUS
            GO                TO   INIT-EXIT
     END-START.
*
     PERFORM TENMS1-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY  NC"＃対象ＤＴ無！_" UPON CONS
         MOVE     4010        TO   PROGRAM-STATUS
         GO                   TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     MOVE      SPACE         TO   WK-MEISAI2.
*    ＣＳＶ（カンマ）セット
     MOVE      ","           TO   MK1 MK2 MK3 MK4 MK5.
*    制御バイトセット
     MOVE X"28"              TO   MM5S MM6S.
     MOVE X"29"              TO   MM5E MM6E.
*    ＮＡＶＳ取引先ＣＤ
     MOVE  TEN-F52           TO   MM1.
*    ＮＡＶＳ取引先名
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   MM5
     ELSE
         MOVE TOK-F02        TO   MM5
     END-IF.
*    ＮＡＶＳ店舗ＣＤ　
     MOVE  TEN-F011          TO   MM2
*    ＮＡＶＳ店舗名
     MOVE  TEN-F02           TO   MM6.
*    Ｄ３６５顧客ＩＤ
     MOVE  SPACE             TO   MM3.
*    Ｄ３６５需要家ＩＤ
     MOVE  SPACE             TO   MM4.
*
     MOVE WK-MEISAI2         TO   CSV-REC.
     WRITE CSV-REC
     ADD   1                 TO   CSV-CNT.
*ストックＮＯ管理ファイル読込
     PERFORM  TENMS1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             HTOKMS  TENMS1  KYKJYKCV.
**
     DISPLAY "# READ-CNT = " READ-CNT    UPON  CONS.
     DISPLAY "# CSV-CNT    " CSV-CNT     UPON  CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ読込（ＳＥＱ）　　　　　　　　　　　　　　      *
****************************************************************
 TENMS1-READ-SEC     SECTION.
*
     MOVE "TENMS1-READ-SEC"     TO   S-NAME.
     READ    TENMS1
             AT  END
             MOVE  "END"     TO   END-FLG
             GO              TO   TENMS1-READ-EXIT
             NOT  AT  END
             ADD   1         TO   READ-CNT
     END-READ.
*件数表示
     IF  READ-CNT(6:3) =  "000" OR  "500"
         DISPLAY "READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*
     IF  TEN-F52  >  PARA-TOKCDE
         MOVE  "END"         TO   END-FLG
         GO                  TO   TENMS1-READ-EXIT
     END-IF.
*
     ADD     1               TO   TAISYO-CNT.
*
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE  TEN-F52                TO   TOK-F01.
     READ  HTOKMS
           INVALID        MOVE "INV"   TO  HTOKMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*****************<<  SKJ0050V   END PROGRAM  >>******************
