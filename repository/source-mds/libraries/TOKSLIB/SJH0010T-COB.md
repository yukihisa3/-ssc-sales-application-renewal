# SJH0010T

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0010T.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　手動受信／再受信処理              *
*    作成日／更新日　　　：　1999/11/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0010T.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/11/08.
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
*ＥＯＳ管理マスタ
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       EOS-F01
                                           EOS-F02
                                           EOS-F03
                       FILE      STATUS    EOS-ST.
*回線環境マスタ
     SELECT  JHMKAIF   ASSIGN    TO        DA-01-VI-JHMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
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
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JHMEOSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMEOSF   OF   XFDLIB
                       JOINING   EOS       AS   PREFIX.
****************************************************************
*    FILE = 回線環境マスタ                                     *
****************************************************************
 FD  JHMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
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
                       COPY      FJH00101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  EOS-ST                   PIC  X(02).
     03  KAI-ST                   PIC  X(02).
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
     03  WK-JYU-TOKCD             PIC  9(08)  VALUE  ZERO.
     03  WK-JYU-DTKBN             PIC  X(02)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  KAI-END                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ                   PIC  9(02)  VALUE  ZERO.
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
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"受信取引先を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"選択した番号は受信対象ではありません。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  5   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-KAISEN.
         05  TBL-SEN1             PIC   N(03).
         05  TBL-SEN2             PIC   N(03).
         05  TBL-SEN3             PIC   N(03).
         05  TBL-SEN4             PIC   N(03).
     03  TABLE1      OCCURS  3.
         05  TABLE2  OCCURS  15.
             07  TBL-SEQ          PIC   9(02).
             07  TBL-MTOKCD       PIC   9(08).
             07  TBL-MSEN         PIC   X(01).
             07  TBL-MDTKBN       PIC   X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  EOS-ERR           PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  KAI-ERR           PIC N(15) VALUE
                        NC"回線環境マスタエラー".
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
 01  LINK-JYUKBN           PIC 9(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JYUKBN.
**************************************************************
 DECLARATIVES.
 EOS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     MOVE        EOS-ST    TO        E-ST.
     MOVE        "JHMEOSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EOS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKAIF.
     MOVE        KAI-ST    TO        E-ST.
     MOVE        "JHMKAIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAI-ERR   UPON      CONS.
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
*****OPEN      INPUT            JHMEOSF.
*****OPEN      INPUT            JHMKAIF.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*回線環境画面表示
     PERFORM   JHMKAIF-READ-SEC.
*マスタ→ワークへ
     PERFORM  MST-WORK-SEC.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*ヘッド入力へ
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
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*受信選択入力(2.2)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*受信起動    (2.4)
         WHEN      "4"       PERFORM   JYUSIN-START-SEC
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
     CLOSE             DSPFILE   HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
 INITD001.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
 INITD002.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00101.
 INITD003.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
 INITD004.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
 INITD005.
*受信／再受信項目名称セット
     IF      LINK-JYUKBN   =  1
             MOVE NC"受　　信"    TO   DSP-TYPE
     ELSE
             MOVE NC"再受信　"    TO   DSP-TYPE
     END-IF.
 INITD006.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
 INITD007.
*ワーク→画面へ
     PERFORM  MST-WORK-SEC.
 INITD008.
     PERFORM  WORK-DSP-SEC.
*照会日付入力へ
     MOVE     "2"           TO          PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
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
                MOVE     1       TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  2  TO  ERR-FLG
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
                  IF    TBL-MTOKCD(C-CNT 1)  =  ZERO
                        MOVE   3     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM DSP-INIT-SEC
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
*             取引先選択チェック                     2.1.1     *
****************************************************************
 BODY-CHK-SEC             SECTION.
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
*取引先コード入力
     IF       DSP-SENTAK   NOT   NUMERIC
     OR       DSP-SENTAK   =     ZERO
              MOVE   4       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SENTAK
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SENTAK
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SENTAK
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SENTAK
     END-IF.
*取引先存在チェック
     MOVE     SPACE          TO  CHK-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
                                      OR CHK-FLG = "CHK"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                                         OR CHK-FLG = "CHK"
           IF  DSP-SENTAK = TBL-SEQ(X Y)
               MOVE    "CHK"       TO       CHK-FLG
               IF  TBL-MTOKCD(X Y) NOT = ZERO
               OR  TBL-MTOKCD(X Y) NUMERIC
                   MOVE TBL-MTOKCD(X Y)    TO WK-JYU-TOKCD
                   MOVE TBL-MDTKBN(X Y)    TO WK-JYU-DTKBN
                   MOVE " " TO EDIT-OPTION OF DSP-SENTAK
                   MOVE " " TO EDIT-CURSOR OF DSP-SENTAK
               ELSE
                   MOVE  5  TO ERR-FLG
                   MOVE "R" TO EDIT-OPTION OF DSP-SENTAK
                   MOVE "C" TO EDIT-CURSOR OF DSP-SENTAK
               END-IF
           END-IF
        END-PERFORM
     END-PERFORM.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN      INPUT    JHMEOSF.
*ＳＥＱ番号セット
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                     ADD     1      TO     WK-SEQ
                     MOVE    WK-SEQ TO     TBL-SEQ(X Y)
             END-PERFORM
     END-PERFORM.
*当日スケジュールマスタを読込みながらワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
                                   OR RD-FLG = "END"
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                                   OR RD-FLG = "END"
                     PERFORM JHMEOSF-READ-SEC
                     IF RD-FLG NOT = "END"
                        MOVE  EOS-F03  TO  TBL-MTOKCD(X Y)
                        MOVE  EOS-F04  TO  TBL-MSEN  (X Y)
                        MOVE  EOS-F01  TO  TBL-MDTKBN(X Y)
                     END-IF
             END-PERFORM
     END-PERFORM.
     CLOSE  JHMEOSF.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE    TBL-SEN1    TO    DSP-SEN1.
     MOVE    TBL-SEN2    TO    DSP-SEN2.
     MOVE    TBL-SEN3    TO    DSP-SEN3.
     MOVE    TBL-SEN4    TO    DSP-SEN4.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
         MOVE    TBL-SEQ(P-CNT Y) TO DSP-SEQ(Y)
         IF  TBL-MTOKCD(P-CNT Y)  NOT =  ZERO
         AND TBL-MTOKCD(P-CNT Y)  NUMERIC
             MOVE    TBL-MTOKCD(P-CNT Y) TO DSP-MTOKCD(Y)
             MOVE    TBL-MSEN(P-CNT Y)   TO DSP-MSEN  (Y)
             MOVE    TBL-MDTKBN(P-CNT Y) TO DSP-MDTKBN(Y)
             MOVE    TBL-MTOKCD(P-CNT Y) TO TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF      HTOKMS-INV-FLG NOT = "INV"
                     MOVE  TOK-F02     TO DSP-MTOKNM(Y)
             ELSE
                     MOVE  ALL NC"＊"  TO DSP-MTOKNM(Y)
             END-IF
             EVALUATE TBL-MSEN(P-CNT Y)
                 WHEN "I" MOVE NC"ＩＳＤＮ" TO DSP-MSENNM(Y)
                 WHEN "T" MOVE NC"公衆回線" TO DSP-MSENNM(Y)
                 WHEN OTHER MOVE ALL NC"＊" TO DSP-MSENNM(Y)
             END-EVALUATE
             EVALUATE TBL-MDTKBN(P-CNT Y)
                 WHEN "01" MOVE NC"発注"    TO  DSP-MDTNM(Y)
                 WHEN "02" MOVE NC"支払"    TO  DSP-MDTNM(Y)
                 WHEN "03" MOVE NC"請求"    TO  DSP-MDTNM(Y)
                 WHEN "04" MOVE NC"受領"    TO  DSP-MDTNM(Y)
                 WHEN "05" MOVE NC"商品"    TO  DSP-MDTNM(Y)
                 WHEN OTHER MOVE ALL NC"＊" TO  DSP-MDTNM(Y)
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
                MOVE    "4"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
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
                  IF    TBL-MTOKCD(C-CNT 1)  =  ZERO
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
*             受信処理スタート                                 *
****************************************************************
 JYUSIN-START-SEC      SECTION.
     MOVE     "JYUSIN-START-SEC"  TO   S-NAME.
*受信取引先表示
     DISPLAY NC"取引先ＣＤ＝" WK-JYU-TOKCD UPON CONS.
     DISPLAY NC"ＤＴ区分　＝" WK-JYU-DTKBN UPON CONS.
*受信処理スタート
     CALL    "SCV0072B"   USING   WK-JYU-DTKBN
                                  WK-JYU-TOKCD
                                  LINK-JYUKBN.
*処理終了へ
     MOVE    "END"         TO         END-FLG.
*
 JYUSIN-START-EXIT.
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
     MOVE          PF-MSG-R(2)        TO   DSP-PFGAID.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00101"          TO   DSP-FMT.
     WRITE    DSP-FJH00101.
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
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00101"           TO   DSP-FMT.
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
     MOVE "M"   TO EDIT-OPTION OF DSP-SENTAK.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SENTAK.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             ＥＯＳ管理マスタ読込み                           *
****************************************************************
 JHMEOSF-READ-SEC      SECTION.
     MOVE     "JHMEOSF-READ-SEC"  TO   S-NAME.
*終了フタグ初期化
     MOVE      SPACE              TO   RD-FLG.
 READ000.
*マスタ読込み
     READ JHMEOSF AT END
          MOVE   "END"            TO   RD-FLG
          GO                      TO   JHMEOSF-READ-EXIT
     END-READ.
*受信対象の取引先のみ有効
     IF      EOS-F01  =   "01" OR "02" OR "04" OR "05"
             CONTINUE
     ELSE
             GO                   TO   READ000
     END-IF.
*
 JHMEOSF-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*****DISPLAY "TOK-F01 = " TOK-F01 UPON CONS.
     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
**********DISPLAY "AAA" UPON CONS
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
**********DISPLAY "BBB" UPON CONS
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             回線環境マスタ読込み                   3.0       *
****************************************************************
 JHMKAIF-READ-SEC      SECTION.
*
     OPEN    INPUT    JHMKAIF.
     MOVE    SPACE     TO     KAI-END.
     MOVE   NC"△"   TO     TBL-SEN1 TBL-SEN2 TBL-SEN3 TBL-SEN4.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
                                      OR KAI-END = "END"
             READ JHMKAIF
                  AT  END
                  MOVE   "END"     TO  KAI-END
                  NOT  AT  END
                  EVALUATE KAI-F01 ALSO KAI-F02  ALSO  KAI-F05
                      WHEN   "I"   ALSO   1   ALSO   0
                      MOVE NC"ＯＫ　"  TO     TBL-SEN1
                      WHEN   "I"   ALSO   1   ALSO   1
                      MOVE NC"使用中"  TO     TBL-SEN1
                      WHEN   "I"   ALSO   2   ALSO   0
                      MOVE NC"ＯＫ　"  TO     TBL-SEN2
                      WHEN   "I"   ALSO   2   ALSO   1
                      MOVE NC"使用中"  TO     TBL-SEN2
                      WHEN   "T"   ALSO   1   ALSO   0
                      MOVE NC"ＯＫ　"  TO     TBL-SEN3
                      WHEN   "T"   ALSO   1   ALSO   1
                      MOVE NC"使用中"  TO     TBL-SEN3
                      WHEN   "T"   ALSO   2   ALSO   0
                      MOVE NC"ＯＫ　"  TO     TBL-SEN4
                      WHEN   "T"   ALSO   2   ALSO   1
                      MOVE NC"使用中"  TO     TBL-SEN4
                  END-EVALUATE
              END-READ
     END-PERFORM.
     CLOSE    JHMKAIF.
*
 JHMKAIF-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SJH0010I >*********************

```
