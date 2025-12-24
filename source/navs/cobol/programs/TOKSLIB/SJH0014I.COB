****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　受信状況照会                      *
*    作成日／更新日　　　：　1999/10/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0014I.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/10/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*当日スケジュールマスタ
     SELECT  JHMTJSF   ASSIGN    TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JHMTJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00141  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"照会日付を正しく入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。　　　".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。　　　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"入力した日付のデータは存在しません。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  5   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-JDATE1               PIC   9(04).
     03  TBL-JDATE2               PIC   9(02).
     03  TBL-JDATE3               PIC   9(02).
     03  TABLE1      OCCURS  3.
         05  TABLE2  OCCURS  16.
             07  TBL-MTIME        PIC   9(04).
             07  TBL-MTOKCD       PIC   9(08).
             07  TBL-MJTMS        PIC   9(04).
             07  TBL-MJTME        PIC   9(04).
             07  TBL-MHTMS        PIC   9(04).
             07  TBL-MHTME        PIC   9(04).
             07  TBL-MJKEN        PIC   9(05).
             07  TBL-MDKEN        PIC   9(05).
             07  TBL-MKEKA        PIC   9(02).
*照会日付退避ワーク
 01  GAMEN-DATE.
     03  GAMEN-YY                 PIC   9(04)  VALUE  ZERO.
     03  GAMEN-MM                 PIC   9(02)  VALUE  ZERO.
     03  GAMEN-DD                 PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
                        NC"当日スケジュールエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JHMTJSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-EXIT.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN      INPUT            JHMTJSF.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*照会日付入力(2.2)
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE   JHMTJSF   HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00141.
*ＦＯＲＭ－ＩＤセット
     MOVE    "FJH00141"           TO   DSP-FORMID.
*ＰＲＯＧＲＡＭ－ＩＤセット
     MOVE    "SJH0014I"           TO   DSP-PGID.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    SYS-DATE(1:4)        TO   DSP-JDATE1.
     MOVE    SYS-DATE(5:2)        TO   DSP-JDATE2.
     MOVE    SYS-DATE(7:2)        TO   DSP-JDATE3.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*照会日付入力へ
     IF       SET-FLG  =  "SET"
              MOVE    "3"         TO   PSW
     ELSE
              MOVE    "2"         TO   PSW
     END-IF.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD-CHK-SEC
                IF        ERR-FLG  =  ZERO
                          MOVE  "3"   TO   PSW
                          MOVE   1    TO   P-CNT
                          PERFORM     WORK-DSP-SEC
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             曜日チェック                           2.1.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*曜日チェック（未入力／範囲外）
     IF       DSP-JDATE1     =   ZERO
     OR       DSP-JDATE2     =   ZERO
     OR       DSP-JDATE3     =   ZERO
     OR       DSP-JDATE1     NOT NUMERIC
     OR       DSP-JDATE2     NOT NUMERIC
     OR       DSP-JDATE3     NOT NUMERIC
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
*日付論理チェック（日付チェックサブルーチン使用）
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     DSP-JDATE1          TO   GAMEN-YY.
     MOVE     DSP-JDATE2          TO   GAMEN-MM.
     MOVE     DSP-JDATE3          TO   GAMEN-DD.
     MOVE     GAMEN-DATE          TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF    LINK-OUT-RET  NOT =  ZERO
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
*マスタからワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
*ワークテーブルクリア
     MOVE     SPACE            TO   TABLE-AREA.
     INITIALIZE                     TABLE-AREA.
*照会日付のセット
     MOVE     DSP-JDATE1       TO   TBL-JDATE1.
     MOVE     DSP-JDATE2       TO   TBL-JDATE2.
     MOVE     DSP-JDATE3       TO   TBL-JDATE3.
*曜日により当日スケジュールマスタスタート
     PERFORM JHMTJSF-START-SEC.
     IF       CHK-FLG  =  "CHK"
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
              GO             TO  MST-WORK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
*
*当日スケジュールマスタ読込み
     PERFORM  JHMTJSF-READ-SEC.
     IF       RD-FLG  =  "END"
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
              GO             TO  MST-WORK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
*当日スケジュールマスタを読込みながらワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
                                   OR GAMEN-DATE NOT = TJS-F01
                                   OR RD-FLG = "END"
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 16
                                   OR GAMEN-DATE NOT = TJS-F01
                                   OR RD-FLG = "END"
                     MOVE TJS-F02 TO TBL-MTIME(X Y)
                     MOVE TJS-F03 TO TBL-MTOKCD(X Y)
                     MOVE TJS-F04 TO TBL-MJTMS(X Y)
                     MOVE TJS-F05 TO TBL-MJTME(X Y)
                     MOVE TJS-F06 TO TBL-MHTMS(X Y)
                     MOVE TJS-F07 TO TBL-MHTME(X Y)
                     MOVE TJS-F08 TO TBL-MJKEN(X Y)
                     MOVE TJS-F09 TO TBL-MDKEN(X Y)
                     MOVE TJS-F11 TO TBL-MKEKA(X Y)
                     PERFORM  JHMTJSF-READ-SEC
             END-PERFORM
     END-PERFORM.
     CLOSE  JHMTJSF.
     OPEN   INPUT JHMTJSF.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE      TBL-JDATE1         TO   DSP-JDATE1.
     MOVE      TBL-JDATE2         TO   DSP-JDATE2.
     MOVE      TBL-JDATE3         TO   DSP-JDATE3.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 16
         IF  TBL-MTIME(P-CNT Y)  NOT =  ZERO
             MOVE    TBL-MTIME(P-CNT Y)  TO DSP-MTIME(Y)
             MOVE    TBL-MTOKCD(P-CNT Y) TO DSP-MTOKCD(Y)
             MOVE    TBL-MTOKCD(P-CNT Y) TO TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF      HTOKMS-INV-FLG  =  SPACE
                     MOVE  TOK-F03     TO DSP-MTOKNM(Y)
             ELSE
                     MOVE  ALL NC"＊"  TO DSP-MTOKNM(Y)
             END-IF
             MOVE    TBL-MJTMS(P-CNT Y)  TO DSP-MJTMS(Y)
             MOVE    TBL-MJTME(P-CNT Y)  TO DSP-MJTME(Y)
             MOVE    TBL-MHTMS(P-CNT Y)  TO DSP-MHTMS(Y)
             MOVE    TBL-MHTME(P-CNT Y)  TO DSP-MHTME(Y)
             MOVE    TBL-MJKEN(P-CNT Y)  TO DSP-MJKEN(Y)
             MOVE    TBL-MDKEN(P-CNT Y)  TO DSP-MDKEN(Y)
             EVALUATE TBL-MKEKA(P-CNT Y)
                 WHEN 0   MOVE NC"未受信"       TO  DSP-MKEKA(Y)
                 WHEN 1   MOVE NC"受信中"       TO  DSP-MKEKA(Y)
                 WHEN 2   MOVE NC"変換中"       TO  DSP-MKEKA(Y)
                 WHEN 3   MOVE NC"伝票更新中"   TO  DSP-MKEKA(Y)
                 WHEN 4   MOVE NC"受信完了"     TO  DSP-MKEKA(Y)
                 WHEN 5   MOVE NC"送信Ｆ無し"   TO  DSP-MKEKA(Y)
                 WHEN 50  MOVE NC"パラ作成Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 51  MOVE NC"待受ＪＢＥ"   TO  DSP-MKEKA(Y)
                 WHEN 52  MOVE NC"待受ＰＧＥ"   TO  DSP-MKEKA(Y)
                 WHEN 53  MOVE NC"ＤＴ存在無"   TO  DSP-MKEKA(Y)
                 WHEN 54  MOVE NC"ＴＩＭＥＥ"   TO  DSP-MKEKA(Y)
                 WHEN 55  MOVE NC"ＤＴＢＫＥ"   TO  DSP-MKEKA(Y)
                 WHEN 56  MOVE NC"受信ＦＣＥ"   TO  DSP-MKEKA(Y)
                 WHEN 57  MOVE NC"受信確認Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 58  MOVE NC"変換パラＥ"   TO  DSP-MKEKA(Y)
                 WHEN 59  MOVE NC"変換ＰＧＥ"   TO  DSP-MKEKA(Y)
                 WHEN 60  MOVE NC"更新時間Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 61  MOVE NC"Ｐ強制終了"   TO  DSP-MKEKA(Y)
                 WHEN 62  MOVE NC"受信ＤＴ無"   TO  DSP-MKEKA(Y)
                 WHEN 63  MOVE NC"変換起動Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 64  MOVE NC"ＰＣ受信中"   TO  DSP-MKEKA(Y)
                 WHEN 65  MOVE NC"取引先ＣＥ"   TO  DSP-MKEKA(Y)
                 WHEN 66  MOVE NC"ＰＣ確認全"   TO  DSP-MKEKA(Y)
                 WHEN 71  MOVE NC"変換起動Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 72  MOVE NC"件数Ｌ　Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 73  MOVE NC"回線Ｃ　Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 74  MOVE NC"変換時間Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 80  MOVE NC"パラ作成Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 81  MOVE NC"パラ送信Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 82  MOVE NC"Ｋ起動　Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 83  MOVE NC"パラ取込Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 84  MOVE NC"相手話中　"   TO  DSP-MKEKA(Y)
                 WHEN 85  MOVE NC"受信エラー"   TO  DSP-MKEKA(Y)
                 WHEN 86  MOVE NC"受信時間Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 87  MOVE NC"受信転送Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 88  MOVE NC"パラ送信Ｅ"   TO  DSP-MKEKA(Y)
                 WHEN 89  MOVE NC"ＧＰＪＢＥ"   TO  DSP-MKEKA(Y)
                 WHEN 90  MOVE NC"話中終了"     TO  DSP-MKEKA(Y)
                 WHEN 91  MOVE NC"送信Ｆ無し"   TO  DSP-MKEKA(Y)
                 WHEN 92  MOVE NC"変換異常"     TO  DSP-MKEKA(Y)
                 WHEN 93  MOVE NC"伝更異常"     TO  DSP-MKEKA(Y)
                 WHEN 99  MOVE NC"受信異常Ｋ"   TO  DSP-MKEKA(Y)
             END-EVALUATE
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  3  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM DSP-INIT-SEC
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                END-IF
*次頁
         WHEN   "F012"
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > 3
                        MOVE   4   TO   ERR-FLG
                ELSE
                  IF    TBL-MTIME(C-CNT 1)  =  ZERO
                        MOVE   4     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM DSP-INIT-SEC
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                  END-IF
                END-IF
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*曜日
         WHEN   "1"    WHEN    "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*明細／確認
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00141"          TO   DSP-FMT.
     WRITE    DSP-FJH00141.
     PERFORM  DSP-SYOKI-SEC.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
     EVALUATE   PSW
*曜日
         WHEN   "2"
                MOVE    "HEAD"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00141"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  曜日
     MOVE "M"   TO EDIT-OPTION OF DSP-JDATE1.
     MOVE "M"   TO EDIT-OPTION OF DSP-JDATE2.
     MOVE "M"   TO EDIT-OPTION OF DSP-JDATE3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-JDATE1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-JDATE2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-JDATE3.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み                     *
****************************************************************
 JHMTJSF-START-SEC     SECTION.
*
     MOVE     "JHMTJSF-START-SEC" TO   S-NAME.
*曜日により基本スケジュールマスタスタート
     MOVE       SPACE          TO     RD-FLG.
     MOVE       SPACE          TO     CHK-FLG.
     MOVE       GAMEN-DATE     TO     TJS-F01.
     MOVE       ZERO           TO     TJS-F02.
     MOVE       ZERO           TO     TJS-F03.
     START  JHMTJSF  KEY  IS  >=  TJS-F01  TJS-F02  TJS-F03
            INVALID
            MOVE    "CHK"      TO     CHK-FLG
     END-START.
*
 JHMTJSF-START-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み                     *
****************************************************************
 JHMTJSF-READ-SEC      SECTION.
     MOVE     "JHMTJSF-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ JHMTJSF NEXT AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*画面曜日以外の時、終了へ
     IF      TJS-F01  >  GAMEN-DATE
             MOVE     "END"       TO   RD-FLG
     END-IF.
*
 JHMTJSF-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SJH0014I >*********************
