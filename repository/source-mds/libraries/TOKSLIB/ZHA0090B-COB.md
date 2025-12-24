# ZHA0090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0090B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　在庫管理システム                      *
*    プログラム　　名：　入荷予定リスト　抽出                  *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　1993.05.18      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         ZHA0090B.
 AUTHOR.             NAV.
 DATE-WRITTEN.       93.05.18.
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

*発注Ｆ
     SELECT   ZHACHDT1       ASSIGN        TO  01-VI-ZHACHDT1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  HAC-F01
                                               HAC-F02
                                               HAC-F03
                                               HAC-F04
                                               HAC-F05
                             FILE STATUS   IS  HAC-STA.

*条件Ｆ
     SELECT   JYOKEN1        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*抽出Ｆ
     SELECT   ZOUTF          ASSIGN        TO  01-S-ZOUTF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  OUT-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     ZHA0090  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*発注Ｆ
 FD  ZHACHDT1.
     COPY   ZHACHDT.
*条件Ｆ
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*抽出Ｆ
 FD  ZOUTF
                     BLOCK  CONTAINS  5    RECORDS.
 01  OUT-REC         PIC  X(200).
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
     03  HAC-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  OUT-STA             PIC  X(02).
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"倉庫コードが違います".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"入荷予定日が違います".
     03  MSG-NO04            PIC  N(20)  VALUE
            NC"取消分が違います".
     03  MSG-NO05            PIC  N(20)  VALUE
            NC"対象データはありません".
     03  MSG-NO06            PIC  N(20)  VALUE
            NC"対象データ抽出中です".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  6   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*月末
 01  MATUBI-TBL.
     03  FILLER              PIC  X(24)  VALUE
                                 "312831303130313130313031".
 01  MATUBI-TBL-R    REDEFINES    MATUBI-TBL.
     03  WK-MATUBI           PIC  9(02)  OCCURS  12  TIMES.
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06).
     03  WK-SYSDTR   REDEFINES    WK-SYSDT.
         05  WK-SYSYY        PIC  9(02).
         05  WK-SYSMM        PIC  9(02).
         05  WK-SYSDD        PIC  9(02).
     03  WK-NYUDT            PIC  9(08).
     03  WK-NYUDTR   REDEFINES    WK-NYUDT.
         05  WK-NYUYY        PIC  9(04).
         05  WK-NYUMM        PIC  9(02).
         05  WK-NYUDD        PIC  9(02).
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(02)  VALUE  ZERO.
     03  PF05-FLG            PIC  9(01)  VALUE  ZERO.
     03  HAC-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SOKOCD           PIC  9(02)  VALUE  ZERO.
     03  WK-SOKONM           PIC  N(18)  VALUE  SPACE.
     03  WK-SHO              PIC  9(04)  VALUE  ZERO.
     03  WK-AMARI            PIC  9(04)  VALUE  ZERO.
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### ZHA0090B".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** ZHA0090B".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** ZHACHDT1 ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-OUTPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** ZOUTF    ".
         05  FILLER          PIC  X(10)  VALUE  " OUTPUT = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-WS               PIC  X(08).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-WS.
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
*発注Ｆ
 HAC-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZHACHDT1.
     MOVE    "ZHACHDT1"    TO    ERR-FL-ID.
     MOVE     HAC-STA      TO    ERR-STCD.
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
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZOUTF.
     MOVE    "ZOUTF"        TO    ERR-FL-ID.
     MOVE     OUT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  ZHA0090B   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  ZHA0090B     END  **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       ZHACHDT1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       JYOKEN1.
     OPEN     OUTPUT      ZOUTF.
*システム日付取得
     ACCEPT  WK-SYSDT     FROM  DATE.
*倉庫ＣＤ取得
     MOVE  "65"         TO  JYO-F01.
     MOVE  LINK-WS      TO  JYO-F02.
     READ  JYOKEN1
       INVALID
         DISPLAY   "JYOKEN1 INV]] KEY = "  UPON   CONS
         DISPLAY   "JYO-F01  = " JYO-F01   UPON   CONS
         DISPLAY   "JYO-F02  = " JYO-F02   UPON   CONS
         MOVE       4000         TO    PROGRAM-STATUS
       NOT INVALID
         MOVE  JYO-F04      TO  WK-SOKOCD
     END-READ.
*倉庫名取得
     IF     WK-SOKOCD  NOT =  1
       MOVE   WK-SOKOCD    TO  SOK-F01
       READ   ZSOKMS1
         INVALID
           DISPLAY   "ZSOKMS1 INV]] KEY = "  UPON   CONS
           DISPLAY   "SOK-F01  = " SOK-F01   UPON   CONS
           DISPLAY   "WKSTN    = " LINK-WS   UPON   CONS
           MOVE       4000         TO    PROGRAM-STATUS
         NOT  INVALID
           MOVE   SOK-F02      TO  WK-SOKONM
       END-READ
     END-IF.
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
     CLOSE      ZHACHDT1.
     CLOSE      ZSOKMS1.
     CLOSE      JYOKEN1.
     CLOSE      ZOUTF.
*
     IF   PF05-FLG  =  ZERO
       DISPLAY    MSG-INPUT    UPON   CONS
       DISPLAY    MSG-OUTPUT   UPON   CONS
     ELSE
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
     MOVE "ZHA0090"     TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-ZHA0090.
     PERFORM  OPT-CLR-SEC.
*倉庫
     IF    WK-SOKOCD NOT = 1
       MOVE  WK-SOKOCD    TO  DSP-SOKOCD
       MOVE  WK-SOKONM    TO  DSP-SOKONM
     END-IF.
*入荷予定日
     MOVE  WK-SYSYY     TO  DSP-NYUYY.
     MOVE  WK-SYSMM     TO  DSP-NYUMM.
     MOVE  WK-SYSDD     TO  DSP-NYUDD.
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
     IF    WK-SOKOCD NOT = 1
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-SOKOCD
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
     IF  WK-SOKOCD   =   1
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
     END-IF.
*入荷予定日
     IF  DSP-NYUMM   =   ZERO
     OR  DSP-NYUMM   >   12
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-NYUMM
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-NYUMM
     END-IF.
     IF  DSP-NYUDD   =   ZERO
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-NYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-NYUDD
     END-IF.
     IF  ERR-FLG     =   3
         GO    TO    CHK-BODY-EXIT
     END-IF.
*閏年ＣＨＫ
     IF  DSP-NYUYY   >  90
         COMPUTE   WK-NYUYY   =  DSP-NYUYY  + 1900
     ELSE
         COMPUTE   WK-NYUYY   =  DSP-NYUYY  + 2000
     END-IF.
     IF  DSP-NYUMM   =  2
         DIVIDE    WK-NYUYY    BY      4
         GIVING    WK-SHO      REMAINDER    WK-AMARI
         IF  WK-AMARI  =  ZERO
             MOVE       29   TO   WK-MATUBI(2)
         ELSE
             MOVE       28   TO   WK-MATUBI(2)
         END-IF
     END-IF.
*月末ＣＨＫ
     IF    DSP-NYUDD >  WK-MATUBI(DSP-NYUMM)
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-NYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-NYUDD
     END-IF.
*入荷予定日をワークへ設定
     IF  ERR-FLG  =  ZERO
         MOVE   DSP-NYUMM    TO  WK-NYUMM
         MOVE   DSP-NYUDD    TO  WK-NYUDD
     END-IF.
*取消分
     IF  DSP-TORIKB  NOT =  SPACE  AND  1
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-TORIKB
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-TORIKB
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
                          IF     ERR-FLG  =  ZERO
                            MOVE   6      TO   ERR-FLG
                            PERFORM  DSP-WT-SEC
                            MOVE  4      TO   MAIN-FLG
                          ELSE
                            MOVE  2      TO   MAIN-FLG
                          END-IF
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
     MOVE  ZERO      TO      ERR-FLG.
*発注Ｆスタート
     MOVE  ZERO              TO  HAC-FLG.
     MOVE  50                TO  HAC-F01.
     MOVE  ZERO              TO  HAC-F02.
     MOVE  ZERO              TO  HAC-F03.
     MOVE  ZERO              TO  HAC-F04.
     MOVE  ZERO              TO  HAC-F05.
     START ZHACHDT1  KEY >=      HAC-F01
                                 HAC-F02
                                 HAC-F03
                                 HAC-F04
                                 HAC-F05
         INVALID
            MOVE   9         TO  HAC-FLG
         NOT  INVALID
*発注Ｆリード
            PERFORM  HAC-RD-SEC
     END-START.
*対象データ存在ＣＨＫ
     IF  HAC-FLG  =  9
         MOVE    5    TO    ERR-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                更新処理
*=============================================================
 UPDT-SEC          SECTION.
*転送
     MOVE     HAC-REC   TO  OUT-REC.
*抽出Ｆ更新
     WRITE    OUT-REC.
     ADD      1         TO  CNT-WRITE.
*発注Ｆリード
     PERFORM   HAC-RD-SEC.
     IF        HAC-FLG  =  9
               MOVE     99      TO   MAIN-FLG
     END-IF.
 UPDT-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理
*=============================================================
 HAC-RD-SEC      SECTION.
*リード
     READ  ZHACHDT1   NEXT
         AT   END
           MOVE   9        TO  HAC-FLG
           GO              TO  HAC-RD-EXIT
     END-READ.
*伝票区分
     IF   HAC-F01  NOT =  50
         MOVE   9          TO  HAC-FLG
         GO                TO  HAC-RD-EXIT
     END-IF.
     IF   HAC-F36  NOT =  1
         GO                TO  HAC-RD-SEC
     END-IF.
*行_
     IF   HAC-F05  =  ZERO
     OR   HAC-F05  >  6
         GO                TO  HAC-RD-SEC
     END-IF.
*枝番，相殺区分，完了区分
     IF   HAC-F03  NOT =  ZERO
     OR   HAC-F04  NOT =  ZERO
     OR   HAC-F06  NOT =  ZERO
         GO                TO  HAC-RD-SEC
     END-IF.
*倉庫ＣＤ
     IF   HAC-F31  NOT  =  DSP-SOKOCD
       GO                TO  HAC-RD-SEC
     END-IF.
*納入予定日
     IF   HAC-F13  NOT  =  WK-NYUDT
         GO                TO  HAC-RD-SEC
     END-IF.
*****IF   HAC-F13  >       WK-NYUDT
*****    GO                TO  HAC-RD-SEC
*****END-IF.
*取消分
     EVALUATE    DSP-TORIKB
       WHEN       " "
          IF   HAC-F37   NOT =  ZERO
             GO                TO  HAC-RD-SEC
          END-IF
       WHEN       "1"
          IF   HAC-F37   NOT =  1
             GO                TO  HAC-RD-SEC
          END-IF
     END-EVALUATE.
*発注ＦＬＧ
     IF   HAC-F36  NOT  =  1
         GO                TO  HAC-RD-SEC
     END-IF.
*発注書ＦＬＧ
     IF   HAC-F42  NOT  =  1
         GO                TO  HAC-RD-SEC
     END-IF.
*カウント
     ADD   1               TO         CNT-READ.
 HAC-RD-EXIT.
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
     WRITE DSP-ZHA0090.
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
                             EDIT-CURSOR  OF  DSP-NYUYY
                             EDIT-CURSOR  OF  DSP-NYUMM
                             EDIT-CURSOR  OF  DSP-NYUDD
                             EDIT-CURSOR  OF  DSP-TORIKB.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SOKOCD
                             EDIT-OPTION  OF  DSP-NYUYY
                             EDIT-OPTION  OF  DSP-NYUMM
                             EDIT-OPTION  OF  DSP-NYUDD
                             EDIT-OPTION  OF  DSP-TORIKB.
 OPT-CLR-EXIT.
     EXIT.

```
