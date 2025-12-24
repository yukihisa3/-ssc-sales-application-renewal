****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信                            *
*    業務名　　　　　　　：　当日スケジュール                  *
*    モジュール名　　　　：　当日スケジュ－ルマスタリスト      *
*    作成日／作成者　　　：　99/09/03  /  HAGIWARA             *
*    更新日／更新者　　　：　99/11/24  /  HAGIWARA             *
*                            日付指定画面追加。                *
*                                                              *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH0007L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/09/03.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
           YA      IS   CHR-2
           YB-21   IS   CHR-21
           YB      IS   CHR-15
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*当日スケジュールマスタ
     SELECT     JHMTJSF    ASSIGN    TO        JHMTJSL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       TJS-F01
                                               TJS-F02
                                               TJS-F03
                           FILE      STATUS    TJS-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*オンライン受信結果マスタ
     SELECT     JHMERRF    ASSIGN    TO        JHMERRL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       ERR-F01
                           FILE      STATUS    ERR-ST.
*画面定義ファイル
     SELECT     DSPFILE    ASSIGN    TO        GS-DSPF
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING          DSP-PRO
                           FUNCTION            DSP-FNC
                           FILE      STATUS    DSP-ST.
*
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        LP-04-PRTF
                           FILE      STATUS    PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
* FILE=当日スケジュールマスタ                              *
****************************************************************
 FD  JHMTJSF
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMTJSF   OF        XFDLIB
     JOINING     TJS       AS        PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************************************************
*  FILE= オンライン受信結果マスタ                              *
****************************************************************
 FD  JHMERRF
*    BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMERRF   OF        XFDLIB
     JOINING     ERR       AS        PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00071  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  TJS-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
     03  ERR-ST            PIC X(02).
     03  DSP-ST            PIC X(02).
     03  PRT-ST            PIC X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT           PIC  X(08).
     03  DSP-GRP           PIC  X(08).
     03  DSP-PRO           PIC  X(02).
     03  DSP-FNC           PIC  X(04).
***  ﾌﾗｸﾞｴﾘｱ
 01  FLG-AREA.
     03  READ-FLG          PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG           PIC  9(02)  VALUE  ZERO.
     03  END-FLG           PIC  X(03)  VALUE  SPACE.
     03  INV-FLG           PIC  9(01)  VALUE  ZERO.
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  PSW               PIC X(01) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
     03  WK-DENNO          PIC 9(08) VALUE     ZERO.
*    03  SYS-DATE          PIC 9(06) VALUE     ZERO.
     03  RD-SW             PIC 9(01) VALUE     ZERO.
*    03  WK-DATE           PIC 9(08) VALUE     ZERO.
     03  SAV-SHORI         PIC 9(01) VALUE     ZERO.
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
                        NC"当日スケジュールマスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  ERR-ERR           PIC N(15) VALUE
                        NC"受信結果マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンターエラー".
***  日付取得
*01  WK-DATE8.
*    03  WK-DATE8-YY1             PIC  9(02).
*    03  WK-DATE8-YY2             PIC  9(06).
*01  WK-DATE8-R         REDEFINES  WK-DATE8.
*    03  WK-YYYY                  PIC  9(04).
*    03  WK-MM                    PIC  9(02).
*    03  WK-DD                    PIC  9(02).
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
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"日付を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"日付論理エラー。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"該当するデータが存在しません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  4   PIC   N(20).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(32)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"※※　当日スケジュールマスタリスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"受信日付：".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD2-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD2-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月".
     03  HD2-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日".
*
 01  HD3.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受信時間"
                                 CHARACTER   TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"取引先"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受信開始"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受信終了"
                                 CHARACTER   TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"変換開始"
                                 CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"変換終了"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受信件数"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"伝票枚数"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"結果"
                                 CHARACTER   TYPE   IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*01  SEN1.
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(36)  VALUE
*        "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-01                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-02                  PIC  99.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-03                  PIC  ZZZZZZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-04                  PIC  N(15).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-05                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-06                  PIC  99.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-07                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-08                  PIC  99.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-09                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-10                  PIC  99.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-11                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-12                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-13                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-14                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-15                  PIC  N(05).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSF.
     DISPLAY     TJS-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TJS-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ERR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMERRF.
     DISPLAY     ERR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
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
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     JHMTJSF   HTOKMS   JHMERRF.
     OPEN     OUTPUT    PRTFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*初期日付
     MOVE    SYS-DATE             TO   DSP-DATE.
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
*    PERFORM   DSP-WRITE-SEC.
*    PERFORM   DSP-READ-SEC.
*
     EVALUATE      PSW
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*リスト出力初期処理
         WHEN      "3"  PERFORM   PRT-INIT-SEC
*リスト出力メイン処理
         WHEN      "4"  PERFORM   PRT-MAIN-SEC
                                  UNTIL READ-FLG = "END"
                        CLOSE     PRTFILE
                        OPEN      OUTPUT    PRTFILE
***                   ワーク初期化
                        MOVE      ZERO  TO  P-CNT     L-CNT
                                            INV-FLG
                        MOVE      SPACE TO  READ-FLG
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*日付チェック
***  日付未入力チェック
     IF       DSP-DATE  NOT NUMERIC
         OR   DSP-DATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           日付論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-DATE       TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   2       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-DATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-DATE
     END-IF.
*当日スケジュールマスタ存在チェック
     PERFORM  FL-START-SEC.
     IF       INV-FLG   =    9
              MOVE      3    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-DATE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-DATE
              MOVE           ZERO      TO    INV-FLG
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "2"   TO    PSW
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-DATE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-DATE
     END-IF.
     CLOSE    JHMTJSF.
     OPEN  INPUT   JHMTJSF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "3"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             リスト出力初期処理(PSW = 3)            2.3       *
****************************************************************
 PRT-INIT-SEC                SECTION.
     MOVE     "PRT-INIT-SEC"    TO   S-NAME.
*当日スケジュールマスタＳＴＡＲＴ
     PERFORM  FL-START-SEC.
     IF       INV-FLG   NOT = ZERO
              MOVE      3         TO   ERR-FLG
              MOVE      ZERO      TO   INV-FLG
              MOVE      "1"       TO   PSW
              GO        TO        PRT-INIT-EXIT
     END-IF.
*当日スケジュールマスタＲＥＡＤ
     PERFORM  FL-READ-SEC.
     IF       READ-FLG  =    "END"
              MOVE      3         TO   ERR-FLG
              MOVE      SPACE     TO   READ-FLG
              MOVE      "1"       TO   PSW
              GO        TO        PRT-INIT-EXIT
     END-IF.
***  MOVE     TJS-F01        TO   WK-DATE.
*１ページ目ヘッダ出力
     PERFORM  HEAD-WRITE-SEC.
*
     MOVE    "4"        TO        PSW.
*
 PRT-INIT-EXIT.
     EXIT.
****************************************************************
*             リスト出力メイン処理( PSW = 4 )        2.4       *
****************************************************************
 PRT-MAIN-SEC                SECTION.
     MOVE     "PRT-MAIN-SEC"    TO   S-NAME.
*
*改ページ判定
     IF       L-CNT     >=   60
         OR   TJS-F01   NOT = DSP-DATE
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
***           MOVE      TJS-F01  TO    WK-DATE
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*
     PERFORM   BODY-WRITE-SEC.
*
 PRT-MAIN-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                       2.4.1     *
****************************************************************
 HEAD-WRITE-SEC                  SECTION.
     MOVE     "HEAD-WRITE-SEC"    TO   S-NAME.
 HEAD010.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
* ヘッダ１行目
***  プログラムＩＤ
     MOVE     "SJH0007L"          TO        HD1-01.
***  日付
     MOVE     SYS-DATE(1:4)       TO        HD1-02.
     MOVE     SYS-DATE(5:2)       TO        HD1-03.
     MOVE     SYS-DATE(7:2)       TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*ヘッダ２行目
***  受信日付
     MOVE     TJS-F01(1:4)        TO        HD2-01.
     MOVE     TJS-F01(5:2)        TO        HD2-02.
     MOVE     TJS-F01(7:2)        TO        HD2-03.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  1.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     MOVE     ZERO         TO        WK-TORICD.
     ADD      5            TO        L-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                       2.4.2     *
****************************************************************
 BODY-WRITE-SEC              SECTION.
     MOVE     "BODY-WRITE-SEC"    TO   S-NAME.
*
*    MOVE        SPACE            TO   DT1.
*一件目のみ取引先マスタより名称を取得
     IF       WK-TORICD  =   TJS-F03
              GO             TO    DT-010
     ELSE
          MOVE   TJS-F03     TO    DT1-03  WK-TORICD
     END-IF.
***  取引先ＲＥＡＤ
     MOVE        TJS-F03     TO    TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE       TO    DT1-04
     NOT INVALID
          MOVE   TOK-F03     TO    DT1-04
     END-READ.
*
 DT-010.
***  受信時間
     MOVE     TJS-F02(1:2)   TO        DT1-01.
     MOVE     TJS-F02(3:2)   TO        DT1-02.
***  受信開始
     MOVE     TJS-F04(1:2)   TO        DT1-05.
     MOVE     TJS-F04(3:2)   TO        DT1-06.
***  受信終了
     MOVE     TJS-F05(1:2)   TO        DT1-07.
     MOVE     TJS-F05(3:2)   TO        DT1-08.
***  変換開始
     MOVE     TJS-F06(1:2)   TO        DT1-09.
     MOVE     TJS-F06(3:2)   TO        DT1-10.
***  変換終了
     MOVE     TJS-F07(1:2)   TO        DT1-11.
     MOVE     TJS-F07(3:2)   TO        DT1-12.
***  受信件数／伝票枚数
     MOVE     TJS-F08        TO        DT1-13.
     MOVE     TJS-F09        TO        DT1-14.
***  結果
     MOVE     TJS-F11        TO        ERR-F01.
     READ     JHMERRF
         INVALID
              MOVE      SPACE     TO   DT1-15
         NOT INVALID
              MOVE      ERR-F05   TO   DT1-15
     END-READ.
*明細部出力
     WRITE    PRT-REC   FROM      DT1       AFTER  1.
*
     ADD      1         TO        L-CNT.
*
     PERFORM      FL-READ-SEC.
*
 BODY-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00071"          TO   DSP-FMT.
     WRITE    DSP-FJH00071.
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
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FJH00071"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00071.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DATE.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタＳＴＡＲＴ                 *
****************************************************************
 FL-START-SEC          SECTION.
     MOVE     "FL-START-SEC"      TO   S-NAME.
*
     MOVE     DSP-DATE       TO   TJS-F01.
     START    JHMTJSF   KEY  IS   >=   TJS-F01
       INVALID
              MOVE      9    TO   INV-FLG
     END-START.
*
 FL-START-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタＲＥＡＤ                   *
****************************************************************
 FL-READ-SEC           SECTION.
     MOVE     "FL-READ-SEC"       TO   S-NAME.
*
     READ     JHMTJSF        AT   END
              MOVE     "END"      TO   READ-FLG
              MOVE     "1"        TO   PSW
              GO        TO        FL-READ-EXIT
     END-READ.
*当日日付が変わったらＥＮＤ
     IF       TJS-F01  NOT =  DSP-DATE
              MOVE     "END"      TO   READ-FLG
              MOVE     "1"        TO   PSW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMTJSF  HTOKMS  JHMERRF  DSPFILE  PRTFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJA0103L   END PROGRAM  >>******************
