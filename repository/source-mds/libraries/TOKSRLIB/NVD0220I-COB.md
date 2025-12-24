# NVD0220I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0220I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷実績確定リスト発行指示　　　　*
*    作成日／作成者　　　：　2020/02/23   ASS.II               *
*    処理内容　　　　　　：　画面より出力条件を入力して　　　　*
*    　　　　　　　　　　　　対象データを抽出し、入荷実績確定　*
*                            リストデータを作成する。　　　　　*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         NVD0220I.
 AUTHOR.             NAV.II.
 DATE-WRITTEN.       02.02.23.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*画面Ｆ
     SELECT   DSPF           ASSIGN               TO  GS-DSPF
                             ORGANIZATION         IS  SEQUENTIAL
                             ACCESS MODE          IS  SEQUENTIAL
                             SYMBOLIC DESTINATION IS "DSP"
                             PROCESSING MODE      IS  DSP-PRO
                             GROUP                IS  DSP-GRP
                             FORMAT               IS  DSP-FMT
                             SELECTED FUNCTION    IS  DSP-FNC
                             FILE STATUS          IS  DSP-STA.

*入庫ファイル
     SELECT   NYKFILF        ASSIGN        TO   DA-01-VI-NYKFILL2
                             ORGANIZATION  IS   INDEXED
                             ACCESS  MODE  IS   SEQUENTIAL
                             RECORD  KEY   IS   NYK-F27
                                                NYK-F02
                                                NYK-F03
                                                NYK-F04
                                                NYK-F05
                             FILE STATUS   IS   NYK-STA.
*発注Ｆ（ヘッダ）
     SELECT   HACHEDF        ASSIGN        TO  DA-01-VI-HACHEDL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  HED-F02
                             FILE STATUS   IS  HED-STA.

*発注Ｆ(明細）
     SELECT   HACMEIF        ASSIGN        TO  DA-01-VI-HACMEIL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  BDY-F21
                                               BDY-F03
                             FILE STATUS   IS  BDY-STA.

*条件Ｆ
     SELECT   JYOKEN1        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  DA-01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*抽出Ｆ
     SELECT   NYKJISF       ASSIGN        TO   DA-01-S-NYKJISF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  NYJ-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FVD02201  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*入庫ファイル
 FD  NYKFILF.
     COPY     NYKFILF  OF  XFDLIB
     JOINING  NYK      AS  PREFIX.
*発注Ｆ（ヘッダ）
 FD  HACHEDF.
     COPY     HACHEDF  OF  XFDLIB
     JOINING  HED      AS  PREFIX.
*発注Ｆ（明細）
 FD  HACMEIF.
     COPY     HACMEIF  OF  XFDLIB
     JOINING  BDY      AS  PREFIX.
*条件Ｆ
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*抽出Ｆ
 FD  NYKJISF
                     BLOCK  CONTAINS  6    RECORDS.
     COPY     NYKJISF OF  XFDLIB
     JOINING  NYJ      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PRO             PIC  X(02).
     03  DSP-GRP             PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-FNC             PIC  X(04).
*ステータス
 01  STA-AREA.
     03  DSP-STA             PIC  X(02).
     03  NYK-STA             PIC  X(02).
     03  HED-STA             PIC  X(02).
     03  BDY-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  NYJ-STA             PIC  X(02).
*カウント
 01  CNT-AREA.
     03  IN-CNT                   PIC  9(07)  VALUE  ZERO.
     03  OT-CNT                   PIC  9(07)  VALUE  ZERO.
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"倉庫コードが違います".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"入荷予定日が違います".
     03  MSG-NO04            PIC  N(20)  VALUE
            NC"計上が違います".
     03  MSG-NO05            PIC  N(20)  VALUE
            NC"対象データはありません".
     03  MSG-NO06            PIC  N(20)  VALUE
            NC"対象データ抽出中です".
     03  MSG-NO07            PIC  N(20)  VALUE
            NC"開始が終了を越えています".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  7   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*
 01  WORK-AREA.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(02)  VALUE  ZERO.
     03  PF05-FLG            PIC  9(01)  VALUE  ZERO.
     03  NYK-FLG             PIC  9(01)  VALUE  ZERO.
 01  WK-BDY-KEY.
     03  WK-BDY-F02          PIC  9(07).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### NVD0220I".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
******************************************************************
 DECLARATIVES.
*画面Ｆ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*入庫ファイル
 NYK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYKFILF.
     MOVE    "NYKFILF2"    TO    ERR-FL-ID.
     MOVE     NYK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*発注Ｆ（ヘッダ）
 HED-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACHEDF.
     MOVE    "HACHEDL2"    TO    ERR-FL-ID.
     MOVE     HED-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*発注Ｆ（明細）
 BDY-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACMEIF.
     MOVE    "HACMEIL2"    TO    ERR-FL-ID.
     MOVE     BDY-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*条件Ｆ
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JYOKEN1.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*抽出Ｆ
 NYJ-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYKJISF.
     MOVE    "NYKJISF"    TO    ERR-FL-ID.
     MOVE     NYJ-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  NVD0220I   START  **"   UPON  CONS.
*************
     MOVE    "01"  TO LINK-SOKCD
     MOVE    "01"  TO LINK-DSOKCD
*
*****DISPLAY "倉庫CD    =" LINK-SOKCD
*****DISPLAY "代表倉庫CD=" LINK-DSOKCD
*************
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  NVD0220I    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       NYKFILF.
     OPEN     INPUT       HACHEDF   HACMEIF.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       JYOKEN1.
     OPEN     OUTPUT      NYKJISF.
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
*特販部名称編集
     MOVE      SPACE              TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     "99"                TO   JYO-F01.
     MOVE     "BUMON"             TO   JYO-F02.
     READ      JYOKEN1
       INVALID KEY
               MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
               MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
*
*初期画面表示へ
     MOVE  1              TO  MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   4      PERFORM  UPDT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF.
     CLOSE      NYKFILF.
     CLOSE      HACHEDF   HACMEIF.
     CLOSE      ZSOKMS1.
     CLOSE      JYOKEN1.
     CLOSE      NYKJISF.
*
*
     DISPLAY "* NYKFILF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* NYKJISF (OT)=" OT-CNT   " *" UPON CONS.
*
*****IF   PF05-FLG NOT =  ZERO
     IF   OT-CNT       =  ZERO
       MOVE  4050       TO  PROGRAM-STATUS
     END-IF.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FVD02201"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FVD02201.
     PERFORM  OPT-CLR-SEC.
*倉庫
     IF     LINK-DSOKCD   NOT =  "01"  AND  "88"
          MOVE   LINK-SOKCD       TO  SOK-F01   DSP-SOKOCD
          READ   ZSOKMS1
            INVALID
              CONTINUE
            NOT  INVALID
              MOVE   SOK-F02      TO  DSP-SOKONM
          END-READ
          MOVE   ZERO             TO  DSP-KEIJYO
     END-IF.
*開始入荷予定日
     MOVE  WK-Y         TO  DSP-SNYUYY  DSP-ENYUYY.
     MOVE  WK-M         TO  DSP-SNYUMM  DSP-ENYUMM.
     MOVE  WK-D         TO  DSP-SNYUDD  DSP-ENYUDD.
*ＢＯＤＹ部入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力処理
*=============================================================
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*倉庫ＣＤプロテクト
     IF    LINK-DSOKCD   NOT =  "01"  AND  "88"
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-SOKOCD
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-KEIJYO
     END-IF.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  9      TO  PF05-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC   SECTION.
*倉庫ＣＤ
     IF  LINK-DSOKCD     =   "01"  OR  "88"
         IF  DSP-SOKOCD  =   SPACE
             MOVE   NC"全倉庫"       TO   DSP-SOKONM
         ELSE
             MOVE   DSP-SOKOCD       TO  SOK-F01
             READ   ZSOKMS1
               INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   2        TO  ERR-FLG
                 END-IF
                 MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKOCD
                 MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKOCD
                 MOVE  SPACE    TO  DSP-SOKONM
               NOT  INVALID  KEY
                 MOVE  SOK-F02  TO  DSP-SOKONM
             END-READ
         END-IF
     END-IF.
*仕入先コード
     IF       DSP-SSIIRE   =  SPACE
              MOVE   SPACE        TO   DSP-SSIIRE
     END-IF.
     IF       DSP-ESIIRE   =  SPACE
              MOVE   99999999     TO   DSP-ESIIRE
     END-IF.
     IF       DSP-SSIIRE   >  DSP-ESIIRE
         IF  ERR-FLG  =  ZERO
             MOVE   7        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSIIRE
                                 EDIT-OPTION  OF  DSP-ESIIRE
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSIIRE
     END-IF.
*開始入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-SNYUYY          TO   WK-SYY.
     MOVE     DSP-SNYUMM          TO   WK-SMM.
     MOVE     DSP-SNYUDD          TO   WK-SDD.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-SYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-SDATE
     ELSE
         MOVE     ZERO            TO   WK-SDATE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
*終了入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-ENYUYY          TO   WK-EYY.
     MOVE     DSP-ENYUMM          TO   WK-EMM.
     MOVE     DSP-ENYUDD          TO   WK-EDD.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-EYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-EDATE
     ELSE
         MOVE     ZERO            TO   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-ENYUYY
     END-IF.
*開始・終了の関連チェック
     IF  WK-SDATE  NOT =  ZERO  AND
         WK-EDATE  NOT =  ZERO  AND
         WK-SDATE     >   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   7        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
                                 EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
*計上
     IF  DSP-KEIJYO  NOT =  0  AND  1  AND  2
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-KEIJYO
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-KEIJYO
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*      2.2       確認入力処理
*=============================================================
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                     TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  9      TO  PF05-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
*=============================================================
*                確認入力チェック（対象データ存在ＣＨＫ）
*=============================================================
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO              TO  ERR-FLG.
     MOVE  ZERO              TO  NYK-FLG.
*入庫Ｆスタート
     INITIALIZE                  NYK-REC.
     MOVE  DSP-SOKOCD        TO  NYK-F27.
     MOVE  ZERO              TO  NYK-F02.
*
     START NYKFILF   KEY >=      NYK-F27  NYK-F02  NYK-F03
                                 NYK-F04  NYK-F05
         INVALID
            MOVE   9         TO  NYK-FLG
         NOT INVALID
            PERFORM  NYK-RD-SEC
     END-START.
*対象データ存在ＣＨＫ
     IF  NYK-FLG  =  9
         MOVE    5    TO    ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE    6    TO    ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE    4    TO    MAIN-FLG
     ELSE
*****    MOVE    2    TO    MAIN-FLG
         MOVE    99   TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*============================================================
*          入庫ファイル　ＲＥＡＤ処理
*============================================================
 NYK-RD-SEC             SECTION.
     READ    NYKFILF
       AT  END
           MOVE     9            TO   NYK-FLG
           GO       TO   NYK-RD-EXIT
     END-READ.
     ADD      1          TO   IN-CNT.
*
     IF   DSP-SOKOCD    NOT =   SPACE     AND
          DSP-SOKOCD    NOT =   NYK-F27
           MOVE     9            TO   NYK-FLG
           GO       TO   NYK-RD-EXIT
     END-IF.
*伝票区分
     IF   LINK-DSOKCD     NOT =   "01"  AND  "88"
          IF    NYK-F01   NOT =   "50"
               GO          TO   NYK-RD-SEC
          END-IF
     END-IF.
*仕入先コード
     IF  DSP-SSIIRE   >    NYK-F08        OR
         DSP-ESIIRE   <    NYK-F08
           GO       TO   NYK-RD-SEC
     END-IF.
*入荷予定日
*****IF  WK-SDATE     >    NYK-F14        OR
*****    WK-EDATE     <    NYK-F14
     IF  WK-SDATE     >    NYK-F26        OR
         WK-EDATE     <    NYK-F26
           GO       TO   NYK-RD-SEC
     END-IF.
*計上
     EVALUATE   DSP-KEIJYO
       WHEN   1
          IF    NYK-F30   =       ZERO
               GO          TO   NYK-RD-SEC
          END-IF
       WHEN   9
          IF    NYK-F30   NOT =   ZERO
               GO          TO   NYK-RD-SEC
          END-IF
     END-EVALUATE.
 NYK-RD-EXIT.
     EXIT.
*=============================================================
*                更新処理
*=============================================================
 UPDT-SEC          SECTION.
*発注ファイル読み込み
     PERFORM   HED-RD-SEC.
     PERFORM   BDY-RD-SEC.
*転送
     MOVE     SPACE     TO  NYJ-REC.
     INITIALIZE             NYJ-REC.
     MOVE     NYK-F27   TO  NYJ-F01.
     MOVE     NYK-F08   TO  NYJ-F02.
     MOVE     NYK-F02   TO  NYJ-F03.
     MOVE     NYK-F05   TO  NYJ-F04.
     MOVE     NYK-F03   TO  NYJ-F05.
     MOVE     NYK-F14   TO  NYJ-F06.
     MOVE     NYK-F26   TO  NYJ-F07.
     MOVE     NYK-F15   TO  NYJ-F08.
     MOVE     NYK-F16   TO  NYJ-F09.
     MOVE     NYK-F17   TO  NYJ-F10.
*****MOVE     BDY-F09   TO  NYJ-F11.
     MOVE     NYK-F25   TO  NYJ-F11.
     MOVE     NYK-F18   TO  NYJ-F12.
     MOVE     NYK-F24   TO  NYJ-F13.
     MOVE     HED-F15   TO  NYJ-F14.
     MOVE     HED-F311  TO  NYJ-F151
     MOVE     HED-F312  TO  NYJ-F152.
     MOVE     NYK-F94   TO  NYJ-F16.
     MOVE     HED-F313  TO  NYJ-F17.
     MOVE     NYK-F91   TO  NYJ-F18.
*抽出Ｆ更新
     WRITE    NYJ-REC.
     ADD      1         TO  OT-CNT.
*発注Ｆリード
     PERFORM   NYK-RD-SEC.
     IF        NYK-FLG  =  9
               MOVE     99      TO   MAIN-FLG
     END-IF.
 UPDT-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理（ヘッダ）
*=============================================================
 HED-RD-SEC      SECTION.
     MOVE     NYK-F02      TO  HED-F02.
     READ  HACHEDF
         INVALID
           INITIALIZE          HED-REC
     END-READ.
 HED-RD-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理（明細）
*=============================================================
 BDY-RD-SEC      SECTION.
     MOVE     NYK-F02      TO  BDY-F02.
     MOVE     NYK-F05      TO  BDY-F03.
     READ  HACMEIF
         INVALID
           INITIALIZE          BDY-REC
     END-READ.
 BDY-RD-EXIT.
     EXIT.
*=============================================================
*                画面表示処理
*=============================================================
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  TBL-MSG(ERR-FLG)    TO  DSP-ERRMSG
     END-IF.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     MOVE  HEN-DATE              TO  DSP-SDATE.
     MOVE  HEN-TIME              TO  DSP-STIME.
     MOVE  HEN-TOKHAN-AREA       TO  DSP-TOKHAN.
     WRITE DSP-FVD02201.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
*=============================================================
*                画面読込処理
*=============================================================
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PRO.
     READ  DSPF.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
*=============================================================
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SOKOCD
                             EDIT-CURSOR  OF  DSP-SSIIRE
                             EDIT-CURSOR  OF  DSP-ESIIRE
                             EDIT-CURSOR  OF  DSP-SNYUYY
                             EDIT-CURSOR  OF  DSP-SNYUMM
                             EDIT-CURSOR  OF  DSP-SNYUDD
                             EDIT-CURSOR  OF  DSP-ENYUYY
                             EDIT-CURSOR  OF  DSP-ENYUMM
                             EDIT-CURSOR  OF  DSP-ENYUDD
                             EDIT-CURSOR  OF  DSP-KEIJYO.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SOKOCD
                             EDIT-OPTION  OF  DSP-SSIIRE
                             EDIT-OPTION  OF  DSP-ESIIRE
                             EDIT-OPTION  OF  DSP-SNYUYY
                             EDIT-OPTION  OF  DSP-SNYUMM
                             EDIT-OPTION  OF  DSP-SNYUDD
                             EDIT-OPTION  OF  DSP-ENYUYY
                             EDIT-OPTION  OF  DSP-ENYUMM
                             EDIT-OPTION  OF  DSP-ENYUDD
                             EDIT-OPTION  OF  DSP-KEIJYO.
 OPT-CLR-EXIT.
     EXIT.

```
