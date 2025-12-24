# FSB0201I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/FSB0201I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　ＦＳＢ０２０１Ｉ                  *
*    作成日／更新日　　　：　2005/09/28 - 10/25 (5)            *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　店舗ＣＤ検索・サブルーチン        *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           FSB0201I.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/09/28.
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
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
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
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSUB0201  OF   XMDLIB
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
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "FSB0201I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FSUB0201".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 20.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 15.
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
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
     03  END-SW                   PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  WK-TORICD                PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD                 PIC  9(05)  VALUE  ZERO.
     03  WK-TORIN                 PIC  N(15)  VALUE  SPACE.
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
*店舗データ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  20.
         05  TABLE2  OCCURS  15.
             07  TBL-SEQ          PIC   9(03).
             07  TBL-TENCD        PIC   9(05).
             07  TBL-TENN         PIC   N(15).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗マスタエラー".
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
**************************************************************
 LINKAGE               SECTION.
 01  LINK-SUBAREA.
   03  LINK-TORICD           PIC 9(08).
   03  LINK-TENCD            PIC 9(05).
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
     OPEN      INPUT            HTENMS.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
     PERFORM   HTOKMS-READ-SEC.
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
     MOVE    SPACE     TO   DSP-FSUB0201.
     MOVE    "SCREEN"  TO   DSP-GRP.
     MOVE    "FSUB0201" TO  DSP-FMT.
     MOVE    "CL"      TO   DSP-PRO.
     WRITE   DSP-FSUB0201.
*ファイル ＣＬＯＳＥ
     CLOSE   HTENMS  DSPFILE   HTOKMS.
**
     IF  END-SW  =  "Y"
         MOVE   ZERO       TO    LINK-TENCD
     END-IF.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示(PSW=1)                     2.1      *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
 INITD001.
*
     MOVE     SPACE               TO   DSP-PRO.
 INITD002.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSUB0201.
 INITD003.
*システム日付・時間転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
***##MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME(1:5)        TO   DSP-STIME.
***##MOVE    WK-TIME(1:4)         TO   DSP-STIME.
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
*********************###PERFORM DSP-INIT-SEC
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
*********************###PERFORM DSP-INIT-SEC
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
                   MOVE TBL-TENCD(X Y)     TO WK-TENCD
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
*ファイルＯＰＥＮ
***##     OPEN      INPUT    HTENMS.
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
*店舗マスタ位置付け
     MOVE     WK-TORICD      TO  TEN-F52.
     MOVE     ZERO           TO  TEN-F011.
     START    HTENMS    KEY  IS  >  TEN-F52  TEN-F011
          INVALID
              MOVE  "END"    TO  RD-FLG
     END-START.
*店舗マスタを読込みワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAX-PGCNT
                                   OR RD-FLG = "END"
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                                   OR RD-FLG = "END"
                     PERFORM  HTENMS-READ-SEC
                     IF RD-FLG NOT = "END"
                        MOVE  TEN-F011  TO  TBL-TENCD(X Y)
                        MOVE  TEN-F02   TO  TBL-TENN(X Y)
                     END-IF
             END-PERFORM
     END-PERFORM.
***##     CLOSE  HTENMS.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                       2.1.1     *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE    WK-TORICD   TO    DSP-TORICD.
     MOVE    WK-TORIN    TO    DSP-TORIN.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
         IF  ( TBL-TENCD(P-CNT Y)  NUMERIC ) AND
             ( TBL-TENCD(P-CNT Y)  NOT = ZERO )
             MOVE    TBL-TENCD(P-CNT Y)  TO  DSP-TENCD(Y)
             MOVE    TBL-TENN(P-CNT Y)   TO  DSP-TENN(Y)
             MOVE    TBL-SEQ(P-CNT Y)    TO  DSP-SEQ(Y)
         ELSE
             MOVE    SPACE    TO  DSP-BDY(Y)
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
*****DISPLAY NC"取引先ＣＤ＝" WK-TORICD    UPON CONS.
*****DISPLAY NC"店舗ＣＤ　＝" WK-TENCD     UPON CONS.
*処理終了へ
     MOVE   WK-TENCD       TO         LINK-TENCD.
     MOVE    "END"         TO         END-FLG.
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
     MOVE    "FSUB0201"          TO   DSP-FMT.
     WRITE    DSP-FSUB0201.
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

     MOVE    "FSUB0201"           TO   DSP-FMT.
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
***  選択番号
     MOVE "M"   TO EDIT-OPTION OF DSP-SENNO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SENNO.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ読込み                                 *
****************************************************************
 HTENMS-READ-SEC      SECTION.
     MOVE     "HTENMS-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ  HTENMS  NEXT   AT END
          MOVE   "END"            TO   RD-FLG
          GO   TO            HTENMS-READ-EXIT
     END-READ.
*選択対象の取引先のみ有効
     IF   TEN-F52  =  WK-TORICD
             CONTINUE
     ELSE
          MOVE   "END"            TO   RD-FLG
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
              MOVE  TOK-F02     TO    WK-TORIN
        ELSE
              MOVE  ALL NC"＊"  TO    WK-TORIN
     END-IF.
*
 HTOKMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END FSB0201I >*********************

```
