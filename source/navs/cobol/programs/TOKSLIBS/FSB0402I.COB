****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　ＦＳＢ０４０２Ｉ                  *
*    作成日／更新日　　　：　2012/01/24                        *
*    作成者／更新者　　　：　ＮＡＶ三浦                        *
*    処理概要　　　　　　：　伝票一覧画面・サブルーチン        *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           FSB0402I.
 AUTHOR.               MIURA.
 DATE-WRITTEN.         12/01/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST.
*----<< 担当者マスタ    >>-*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE      STATUS    TAN-ST.
*----<< 伝票データ >>-*
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN-F01   DEN-F23
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-ST.
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
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*----<< 条件ファイル >>-*
****************************************************************
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
****************************************************************
*----<< 担当者マスタ   >>-*
****************************************************************
 FD  HTANMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTANMS    OF   XFDLIB    JOINING   TAN  AS   PREFIX.
****************************************************************
*----<< 伝票データ >>-*
****************************************************************
 FD  SHTDENF            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     SHTDENF   OF   XFDLIB    JOINING   DEN  AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSUB0401  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  DEN-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "FSB0401I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FSUB0401".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 20.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 12.
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  JYOKEN-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(02)  VALUE  ZERO.
     03  S-CNT                    PIC  9(02)  VALUE  ZERO.
     03  C-CNT                    PIC  9(02)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ                   PIC  9(03)  VALUE  ZERO.
     03  GYO-CNT                  PIC  9(03)  VALUE  ZERO.
     03  END-SW                   PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  WK-BUMON                 PIC  X(04)  VALUE  SPACE.
     03  WK-TORICD                PIC  9(08)  VALUE  ZERO.
     03  WK-NOHIN                 PIC  X(06)  VALUE  SPACE.
     03  WK-TENCD                 PIC  9(05)  VALUE  ZERO.
     03  WK-TENN                  PIC  N(10)  VALUE  SPACE.
     03  WK-DEBA                  PIC  X(02)  VALUE  SPACE.
     03  WK-DEBAN                 PIC  N(05)  VALUE  SPACE.
     03  WK-DENBA                 PIC  X(02)  VALUE  SPACE.
     03  WK-DENBAN                PIC  N(05)  VALUE  SPACE.
     03  WK-TORIN                 PIC  N(10)  VALUE  SPACE.
     03  WK-TANN                  PIC  N(05)  VALUE  SPACE.
     03  WK-TANCD                 PIC  X(02)  VALUE  SPACE.
 01  WK-KEYAREA.
***  伝票キーエリア（画面遷移制御）
     03  WK-F01                   PIC  9(08)  VALUE  ZERO.
     03  WK-F23                   PIC  9(05)  VALUE  ZERO.
     03  WK-F04                   PIC  9(01)  VALUE  ZERO.
     03  WK-F051                  PIC  9(02)  VALUE  ZERO.
     03  WK-F07                   PIC  9(05)  VALUE  ZERO.
     03  WK-F112                  PIC  9(09)  VALUE  ZERO.
 01  WK-KEYAREA2.
     03  WK2-F01                   PIC  9(08)  VALUE  ZERO.
     03  WK2-F23                   PIC  9(05)  VALUE  ZERO.
     03  WK2-F04                   PIC  9(01)  VALUE  ZERO.
     03  WK2-F051                  PIC  9(02)  VALUE  ZERO.
     03  WK2-F07                   PIC  9(05)  VALUE  ZERO.
     03  WK2-F112                  PIC  9(09)  VALUE  ZERO.
*納品日　　
 01  NOHIN-AREA.
     03  WK-NOHINBI               PIC  9(08)  VALUE  ZERO.
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
      VALUE NC"_取消_終了　　　　　　　　".
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
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"選択番号を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"選択した番号は対象ではありません。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  5   PIC   N(20).
*明細データ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  20.
         05  TABLE2  OCCURS  12.
             07  TBL-SEQ          PIC   9(03).
             07  TBL-NOHIN        PIC   X(06).
             07  TBL-TENCD        PIC   9(05).
             07  TBL-TENN         PIC   N(10).
             07  TBL-DEBA         PIC   X(02).
             07  TBL-DEBAN        PIC   N(04).
             07  TBL-DENBA        PIC   X(02).
             07  TBL-DENBAN       PIC   N(04).
             07  TBL-GYOSU        PIC   9(03).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件マスタエラー".
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
**************************************************************
 LINKAGE               SECTION.
 01  LINK-SUBAREA.
   03  LINK-BUMON           PIC X(04).
   03  LINK-TANCD           PIC X(02).
   03  LINK-TORICD          PIC 9(08).
   03  LINK-SOSAI           PIC 9(01).
   03  LINK-DENNO           PIC 9(09).
   03  LINK-DENKU           PIC 9(02).
   03  LINK-OUT-NOHIN       PIC X(06).
   03  LINK-OUT-TENCD       PIC 9(05).
   03  LINK-OUT-DEBA        PIC X(02).
   03  LINK-OUT-DENBA       PIC X(02).
*
**************************************************************
 PROCEDURE             DIVISION USING LINK-SUBAREA.
**************************************************************
 DECLARATIVES.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     MOVE        TEN-ST    TO        E-ST.
     MOVE        "HTENMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TEN-ERR   UPON      CONS.
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
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     MOVE        TAN-ST    TO        E-ST.
     MOVE        "HTANMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "HJYOKEN"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
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
*
 CONTROL-EXIT.
     EXIT  PROGRAM.
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
     OPEN      INPUT            HTENMS HJYOKEN HTANMS.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS SHTDENF.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
     PERFORM   HTOKMS-READ-SEC.
     PERFORM   HTANMS-READ-SEC.
*マスタ→ワークへ
     PERFORM  MST-WORK-SEC.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     MOVE    "1"                  TO   PSW.
*頁セット
     MOVE     1                   TO   P-CNT.
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
*初期画面処理(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*選択番号入力(2.2)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*メインへ遷移(2.4)
         WHEN      "4"       PERFORM   RETURN-SEC
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
**全画面クリア
     MOVE    SPACE     TO   DSP-PRO.
     MOVE    SPACE     TO   DSP-FSUB0401.
     MOVE    "SCREEN"  TO   DSP-GRP.
     MOVE    "FSUB0401" TO  DSP-FMT.
     MOVE    "CL"      TO   DSP-PRO.
     WRITE   DSP-FSUB0401.
*ファイル ＣＬＯＳＥ
     CLOSE   HTENMS DSPFILE HTOKMS SHTDENF HJYOKEN HTANMS.
**
     IF  END-SW  =  "Y"
         MOVE   SPACE      TO    LINK-OUT-NOHIN
         MOVE   ZERO       TO    LINK-OUT-TENCD
         MOVE   SPACE      TO    LINK-OUT-DEBA
         MOVE   SPACE      TO    LINK-OUT-DENBA
     END-IF.
*    DISPLAY  "FSB0401-OUT" LINK-SUBAREA UPON CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示(PSW=1)                     2.1      *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*    DISPLAY  "FSB0401-IN" LINK-SUBAREA UPON CONS.
 INITD001.
*
     MOVE     SPACE               TO   DSP-PRO.
 INITD002.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSUB0401.
 INITD003.
*システム日付・時間転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME(1:5)        TO   DSP-STIME.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORM.
 INITD011.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
 INITD012.
*ワーク→画面へ
     PERFORM  MST-WORK-SEC.
     PERFORM  WORK-DSP-SEC.
*選択番号入力へ
     MOVE     "2"      TO    PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             選択番号入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
                IF        ERR-FLG  =  ZERO
                          MOVE  "3"   TO   PSW
                          PERFORM     WORK-DSP-SEC
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE    1        TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  2  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                END-IF
*次頁
         WHEN   "F012"
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   3   TO   ERR-FLG
                ELSE
                  IF    TBL-TENCD(C-CNT 1)  =  ZERO
                        MOVE   3     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                  END-IF
                END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             店舗選択チェック                       2.2.1     *
****************************************************************
 BODY-CHK-SEC             SECTION.
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
*選択番号入力
     IF       DSP-SENNO   NOT   NUMERIC
     OR       DSP-SENNO   =     ZERO
              MOVE   4       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SENNO
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SENNO
              GO  TO        BODY-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SENNO
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SENNO
     END-IF.
*店舗選択チェック
     MOVE     SPACE          TO  CHK-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAX-PGCNT
                                      OR CHK-FLG = "CHK"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                                         OR CHK-FLG = "CHK"
           IF  DSP-SENNO = TBL-SEQ(X Y)
               MOVE    "CHK"       TO       CHK-FLG
               IF ( TBL-TENCD(X Y) NUMERIC ) AND
                  ( TBL-TENCD(X Y) NOT = ZERO )
                   MOVE TBL-NOHIN(X Y)     TO WK-NOHIN
                   MOVE TBL-TENCD(X Y)     TO WK-TENCD
                   MOVE TBL-DEBA(X Y)      TO WK-DEBA
                   MOVE TBL-DENBA(X Y)     TO WK-DENBA
                   MOVE "M"   TO EDIT-OPTION OF DSP-SENNO
                   MOVE SPACE TO EDIT-CURSOR OF DSP-SENNO
               ELSE
                   MOVE  5  TO ERR-FLG
                   MOVE "R" TO EDIT-OPTION OF DSP-SENNO
                   MOVE "C" TO EDIT-CURSOR OF DSP-SENNO
               END-IF
           END-IF
        END-PERFORM
     END-PERFORM.
*
     IF   CHK-FLG  =  SPACE
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SENNO
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SENNO
     END-IF.
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                 1.1.1     *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
     INITIALIZE    WK-KEYAREA WK-KEYAREA2.
*ファイルＯＰＥＮ
*ＳＥＱ番号セット
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAX-PGCNT
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                     INITIALIZE     TABLE2(X Y)
                     ADD     1      TO     WK-SEQ
                     MOVE    WK-SEQ TO     TBL-SEQ(X Y)
             END-PERFORM
     END-PERFORM.
*伝票ファイル位置付け
     MOVE     LINK-TORICD        TO   DEN-F01.
     MOVE     LINK-DENNO         TO   DEN-F23.
     MOVE     LINK-SOSAI         TO   DEN-F04.
     MOVE     LINK-DENKU         TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F07.
     MOVE     ZERO               TO   DEN-F112.
     MOVE     ZERO               TO   DEN-F03.
     START    SHTDENF   KEY  NOT  <  DEN-F01     DEN-F23
                                     DEN-F04     DEN-F051
                                     DEN-F07     DEN-F112
                                     DEN-F03
          INVALID
              MOVE  "END"    TO  RD-FLG
     END-START.
*伝票ファイルを読込みワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAX-PGCNT
                                   OR RD-FLG = "END"
         PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                                   OR RD-FLG = "END"
                 PERFORM  SHTDENF-READ-SEC
                 IF RD-FLG NOT = "END"
                        MOVE  DEN-F01  TO  WK-F01
                        MOVE  DEN-F23  TO  WK-F23
                        MOVE  DEN-F04  TO  WK-F04
                        MOVE  DEN-F051 TO  WK-F051
                        MOVE  DEN-F07  TO  WK-F07
                        MOVE  DEN-F112 TO  WK-F112
*伝票ファイルキーをセット
                     IF WK-KEYAREA  = WK-KEYAREA2
                        ADD   1        TO  GYO-CNT
                        ADD   -1       TO  Y
                        MOVE  GYO-CNT  TO  TBL-GYOSU(X Y)
                     ELSE
                        PERFORM  HTENMS-READ-SEC
                        PERFORM  DEBA-READ-SEC
                        PERFORM  DENBA-READ-SEC
                        MOVE  WK-KEYAREA TO  WK-KEYAREA2
                        MOVE  1        TO  GYO-CNT
                  MOVE  DEN-F112 TO  WK-NOHINBI
                  MOVE  NOHIN-AREA(3:6) TO  TBL-NOHIN(X Y)
                        MOVE  DEN-F07   TO  TBL-TENCD(X Y)
                        MOVE  WK-TENN   TO  TBL-TENN(X Y)
                        MOVE  DEN-F08   TO  TBL-DEBA(X Y)
                        MOVE  WK-DEBAN  TO  TBL-DEBAN(X Y)
                        MOVE  DEN-F09   TO  TBL-DENBA(X Y)
                        MOVE  WK-DENBAN TO  TBL-DENBAN(X Y)
                        MOVE  GYO-CNT   TO  TBL-GYOSU(X Y)
                     END-IF
                 END-IF
         END-PERFORM
     END-PERFORM.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                       2.1.1     *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE    WK-BUMON    TO    DSP-BMNCD.
     MOVE    WK-TANCD    TO    DSP-TANCD.
     MOVE    WK-TANN     TO    DSP-TANN.
     MOVE    WK-TORICD   TO    DSP-TORICD.
     MOVE    WK-TORIN    TO    DSP-TORIN.
     MOVE    LINK-SOSAI  TO    DSP-SOSAI.
     MOVE    LINK-DENNO  TO    DSP-DENNO.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
         IF  ( TBL-TENCD(P-CNT Y)  NUMERIC ) AND
             ( TBL-TENCD(P-CNT Y)  NOT = ZERO )
             MOVE    TBL-SEQ(P-CNT Y)     TO  DSP-SEQ(Y)
             MOVE    TBL-NOHIN(P-CNT Y)   TO  DSP-NOHIN(Y)
             MOVE    TBL-TENCD(P-CNT Y)   TO  DSP-TENCD(Y)
             MOVE    TBL-TENN(P-CNT Y)    TO  DSP-TENN(Y)
             MOVE    TBL-DEBA(P-CNT Y)    TO  DSP-DEBA(Y)
             MOVE    TBL-DEBAN(P-CNT Y)   TO  DSP-DEBAN(Y)
             MOVE    TBL-DENBA(P-CNT Y)   TO  DSP-DENBA(Y)
             MOVE    TBL-DENBAN(P-CNT Y)  TO  DSP-DENBAN(Y)
             MOVE    TBL-GYOSU(P-CNT Y)   TO  DSP-GYOSU(Y)
         ELSE
             MOVE    SPACE    TO  DSP-MAS001(Y)
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.3
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
                MOVE    "4"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             メインへ戻り値(PSW=4)                   2.4      *
****************************************************************
 RETURN-SEC              SECTION.
     MOVE     "RETURNT-SEC"  TO   S-NAME.
*選択店舗表示
*処理終了へ
     MOVE   WK-NOHIN       TO     LINK-OUT-NOHIN.
     MOVE   WK-TENCD       TO     LINK-OUT-TENCD.
     MOVE   WK-DEBA        TO     LINK-OUT-DEBA.
     MOVE   WK-DENBA       TO     LINK-OUT-DENBA.
     MOVE    "END"         TO     END-FLG.
*
 RETURN-EXIT.
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
     IF  PSW  =  "2"
         MOVE      PF-MSG-R(2)        TO   DSP-GUIDE
     ELSE
         MOVE      PF-MSG-R(1)        TO   DSP-GUIDE
     END-IF.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSUB0401"          TO   DSP-FMT.
     WRITE    DSP-FSUB0401.
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
*選択
         WHEN   "2"
                MOVE    "SENTAK"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSUB0401"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
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
***  選択番号
     MOVE "M"   TO EDIT-OPTION OF DSP-SENNO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SENNO.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ読込み                                 *
****************************************************************
 HTENMS-READ-SEC       SECTION.
*
     MOVE     "HTENMS-READ-SEC"   TO   S-NAME.
     MOVE   DEN-F07   TO   TEN-F011.
     MOVE   LINK-TORICD  TO   TEN-F52.
     READ   HTENMS  INVALID
              MOVE    "INV"     TO    HTENMS-INV-FLG
          NOT  INVALID
              MOVE    SPACE     TO    HTENMS-INV-FLG
     END-READ.
     IF      HTENMS-INV-FLG NOT = "INV"
              MOVE  TEN-F03     TO    WK-TENN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-TENN
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                               *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
     MOVE   LINK-TORICD   TO   TOK-F01.
     MOVE   LINK-TORICD   TO   WK-TORICD.
     READ   HTOKMS  INVALID
              MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
              MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
     IF      HTOKMS-INV-FLG NOT = "INV"
              MOVE  TOK-F03     TO    WK-TORIN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-TORIN
     END-IF.
*
 HTOKMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   担当者マスタ　　　ＲＥＡＤ　                 *
*--------------------------------------------------------------*
 HTANMS-READ-SEC            SECTION.
     MOVE     "HTANMS-READ-SEC"   TO   S-NAME.
     MOVE   LINK-TANCD    TO   TAN-F02 WK-TANCD.
     MOVE   LINK-BUMON    TO   TAN-F01 WK-BUMON.
     READ   HTANMS  INVALID
              MOVE    "INV"     TO    HTANMS-INV-FLG
          NOT  INVALID
              MOVE    SPACE     TO    HTANMS-INV-FLG
     END-READ.
     IF      HTANMS-INV-FLG NOT = "INV"
              MOVE  TAN-F03(1:10)  TO    WK-TANN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-TANN
     END-IF.
 TAN-RD-EXIT.
****************************************************************
*             伝票ファイル読込み
****************************************************************
 SHTDENF-READ-SEC      SECTION.
     MOVE     "SHTDENF-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ  SHTDENF  NEXT   AT END
          MOVE   "END"            TO   RD-FLG
          GO   TO            SHTDENF-READ-EXIT
     END-READ.
*
     IF (DEN-F01 NOT = LINK-TORICD) OR
        (DEN-F23 NOT = LINK-DENNO ) OR
        (DEN-F04 NOT = LINK-SOSAI) OR
        (DEN-F051 NOT = LINK-DENKU)
        MOVE   "END"            TO   RD-FLG
     END-IF.
 SHTDENF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル（出荷場所）　　ＲＥＡＤ         *
*--------------------------------------------------------------*
 DEBA-READ-SEC            SECTION.
     MOVE     "DEBA-READ-SEC"   TO   S-NAME.
              MOVE      "20"           TO   JYO-F01
              MOVE      DEN-F08         TO   JYO-F02
     READ   HJYOKEN  INVALID
              MOVE    "INV"     TO    JYOKEN-INV-FLG
          NOT  INVALID
              MOVE    SPACE     TO    JYOKEN-INV-FLG
     END-READ.
     IF      JYOKEN-INV-FLG NOT = "INV"
              MOVE  JYO-F03     TO    WK-DEBAN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-DEBAN
     END-IF.
 DEBA-RD-EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル（伝票場所）　　ＲＥＡＤ         *
*--------------------------------------------------------------*
 DENBA-READ-SEC            SECTION.
     MOVE     "DENBA-READ-SEC"   TO   S-NAME.
              MOVE      "20"           TO   JYO-F01
              MOVE      DEN-F09         TO   JYO-F02
     READ   HJYOKEN  INVALID
              MOVE    "INV"     TO    JYOKEN-INV-FLG
          NOT  INVALID
              MOVE    SPACE     TO    JYOKEN-INV-FLG
     END-READ.
     IF      JYOKEN-INV-FLG NOT = "INV"
              MOVE  JYO-F03     TO    WK-DENBAN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-DENBAN
     END-IF.
 DEBAN-RD-EXIT.
*******************< PROGRAM-END FSB0401I >*********************
