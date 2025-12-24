****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　手書出荷依頼リスト　　　　        *
*    作成日／更新日　　　：　2011/04/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取後、出荷指示Ｆを読　*
*                            み、手書出荷依頼リストを出力する。*
*                            　　　　　　　　　　　　　　　　　*
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SKZ0040L.
 AUTHOR.                 NAV.
 DATE-WRITTEN.           11.04.26.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
 INPUT-OUTPUT            SECTION.
 FILE-CONTROL.
*----<< 出荷状況管理ファイル >>-*
     SELECT   SYUJISL1  ASSIGN    TO        DA-01-VI-SYUJISL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JIS-F01   JIS-F02
                                            JIS-F03   JIS-F04
                                            JIS-F05   JIS-F06
                        FILE      STATUS    JIS-ST.
*----<< 担当者マスタ >>-*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE      STATUS    TAN-ST.
*----<< 取引先マスタ >>-*
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION         INDEXED
                       ACCESS    MODE       DYNAMIC
                       RECORD    KEY        TOK-F01
                       FILE      STATUS     TOK-ST.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST.
*----<< プリントファイル >>-*
     SELECT  PRTF      ASSIGN    TO             GS-XU04LP
                       DESTINATION              "PRT"
                       FORMAT                   PRT-FORM
                       GROUP                    PRT-GRP
                       PROCESSING               PRT-PROC
                       UNIT CONTROL             PRT-CTL
                       FILE STATUS              PRT-ST.
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*----<< 出荷状況管理ファイル >>-*
 FD  SYUJISL1           LABEL  RECORD  IS  STANDARD.
     COPY     SYUJISF   OF     XFDLIB
              JOINING   JIS AS PREFIX.
*----<< 担当者マスタ >>-*                                      *
 FD  HTANMS             LABEL  RECORD  IS  STANDARD.
     COPY     HTANMS    OF     XFDLIB
              JOINING   TAN AS PREFIX.
*----<< 取引先マスタ >>-*                                      *
 FD  HTOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     HTOKMS    OF     XFDLIB
              JOINING   TOK AS PREFIX.
*----<< 倉庫マスタ >>-*                                        *
 FD  ZSOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     ZSOKMS1   OF     XFDLIB
              JOINING   SOK AS PREFIX.
*----<< プリントファイル >>-*
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FKZ00401  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  JIS-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*    ＦＯＲＭ制御領域
 01  PRT-FORM                    PIC  X(08).
 01  PRT-PROC                    PIC  X(02).
 01  PRT-GRP                     PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL               PIC  X(04).
     03  PRT-STR-PG              PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
*    退避エリア
 01  WORK-AREA.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TAISYO-CNT               PIC  9(07)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(07)  VALUE  ZERO.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  X                        PIC  9(01)  VALUE  ZERO.
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*日付／時刻
 01  TIME-AREA.
     03  WRK-TIME                 PIC  9(08)  VALUE  ZERO.
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(06)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(08)  VALUE  ZERO.
*納品日編集
 01  NOU-DATE.
     03  NOU-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  NOU-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  NOU-DATE-DD              PIC  9(02)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JIS-ERR           PIC N(15) VALUE
                        NC"出荷状況管理ファイル１エラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタファイルエラー".
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SKZ0040L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                 SECTION.
 01  LINK-BTDATE           PIC 9(08).
 01  LINK-BTTIME           PIC 9(04).
 01  LINK-BTTORI           PIC 9(08).
 01  LINK-SOKCD            PIC X(02).
 01  LINK-BUMON            PIC X(04).
 01  LINK-TANCD            PIC X(02).
*============================================================*
 PROCEDURE               DIVISION
                         USING     LINK-BTDATE
                                   LINK-BTTIME
                                   LINK-BTTORI
                                   LINK-SOKCD
                                   LINK-BUMON
                                   LINK-TANCD.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 PRT-ERR                SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTF.
     MOVE      "PRTF"           TO   ERR-FL-ID.
     MOVE      PRT-ST           TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   JIS-ERR          UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
 JIS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUJISL1.
     MOVE       "SYUJISL1"      TO   ERR-FL-ID.
     MOVE        JIS-ST         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   JIS-ERR          UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE       "TOKMS1"        TO   ERR-FL-ID.
     MOVE        TOK-ST         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   JIS-ERR          UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE       "ZSOKMS1"       TO   ERR-FL-ID.
     MOVE        SOK-ST         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   JIS-ERR          UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     MOVE       "TANMS1"        TO   ERR-FL-ID.
     MOVE        TAN-ST         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   JIS-ERR          UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造_0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
     MOVE     "COTROL-SEC"        TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    FLG-END  =  "END".
     PERFORM  END-SEC.
     STOP     RUN.
*
 CONTROL-EXIT.
     EXIT.
*============================================================*
*　　初期処理　　　　　　　　　　　　　　  構造_1.0         *
*============================================================*
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT      SYUJISL1  HTANMS  HTOKMS  ZSOKMS.
     OPEN     OUTPUT     PRTF.
*    システム日付の取得
*システム日付・時刻の取得
     ACCEPT   WRK-DATE1         FROM   DATE.
     ACCEPT   WRK-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WRK-DATE1           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE2.
*    プリントエリア初期化
     MOVE     SPACE      TO       PRT-FKZ00401.
     MOVE     ZERO       TO       TAISYO-CNT.
*    出荷指示ファイルスタート
     MOVE     SPACE      TO       JIS-REC.
     INITIALIZE                   JIS-REC.
*
     MOVE     "2"        TO       JIS-F01.
     MOVE  LINK-BTDATE   TO       JIS-F02.
     MOVE  LINK-BTTIME   TO       JIS-F03.
     MOVE  LINK-BTTORI   TO       JIS-F04.
     MOVE  LINK-SOKCD    TO       JIS-F05.
     START SYUJISL1  KEY  IS  >=  JIS-F01  JIS-F02  JIS-F03
                                  JIS-F04  JIS-F05  JIS-F06
           INVALID
           DISPLAY NC"＃＃出力対象無１＃＃"  UPON CONS
           STOP  RUN
     END-START.
*    出荷指示ファイル読込
     PERFORM  SYUJISL1-READ-SEC.
*
     IF       FLG-END    =        "END"
              DISPLAY NC"＃＃出力対象無２＃＃"  UPON CONS
              STOP  RUN
     END-IF.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　出荷指示ファイル読込
*============================================================*
 SYUJISL1-READ-SEC       SECTION.
*
     MOVE     "SYUJISL1-READ-SEC"   TO   S-NAME.
*    配送伝票データ読込み
     READ     SYUJISL1  NEXT  AT  END
              MOVE     "END"      TO       FLG-END
              GO                  TO       SYUJISL1-READ-EXIT
     END-READ.
 READ010.
     ADD      1                   TO       READ-CNT.
 READ020.
*    バッチ番号、倉庫確認
     IF       LINK-BTDATE  =  JIS-F02
     AND      LINK-BTTIME  =  JIS-F03
     AND      LINK-BTTORI  =  JIS-F04
     AND      LINK-SOKCD   =  JIS-F05
              CONTINUE
     ELSE
              MOVE     "END"      TO       FLG-END
              GO                  TO       SYUJISL1-READ-EXIT
     END-IF.
*    対象データ件数カウント
     ADD      1                   TO       TAISYO-CNT.
*
 SYUJISL1-READ-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*    システム日付セット
     MOVE      WRK-DATE2(1:4)     TO   PRT-HDYY.
     MOVE      WRK-DATE2(5:2)     TO   PRT-HDMM.
     MOVE      WRK-DATE2(7:2)     TO   PRT-HDDD.
*    頁セット
     ADD       1                  TO   PAGE-CNT.
     MOVE      PAGE-CNT           TO   PRT-HDPAGE.
*倉庫ＣＤセット
     MOVE      JIS-F05            TO   SOK-F01  PRT-SOKCD.
     PERFORM   ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG = SPACE
         MOVE  SOK-F02            TO   PRT-SOKNM
     ELSE
         MOVE  ALL NC"＊"         TO   PRT-SOKNM
     END-IF.
*依頼担当者セット
     MOVE      LINK-BUMON         TO   TAN-F01.
     MOVE      LINK-TANCD         TO   TAN-F02  PRT-TANCD.
     PERFORM   HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG = SPACE
         MOVE  TAN-F03            TO   PRT-TANNM
     ELSE
         MOVE  ALL NC"＊"         TO   PRT-TANNM
     END-IF.
*バッチ番号セット
     MOVE      JIS-F02            TO   PRT-BTDATE.
     MOVE      JIS-F03            TO   PRT-BTTIME.
     MOVE      JIS-F04            TO   PRT-BTTORI.
*取引先名称セット
     MOVE      JIS-F04            TO   TOK-F01  PRT-TORICD
     PERFORM   HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = SPACE
         MOVE  TOK-F02            TO   PRT-TORINM
     ELSE
         MOVE  ALL NC"＊"         TO   PRT-TORINM
     END-IF.
*明細初期化
     MOVE      SPACE              TO   PRT-MAS002.
*明細をセットする。
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X       >  5
                                      OR  FLG-END = "END"
         MOVE  JIS-F06(1:4)       TO   NOU-DATE-YYYY
         MOVE  JIS-F06(5:2)       TO   NOU-DATE-MM
         MOVE  JIS-F06(7:2)       TO   NOU-DATE-DD
         MOVE  NOU-DATE           TO   PRT-NOUDT(X)
         MOVE  JIS-F07            TO   PRT-SDENNO(X)
         MOVE  JIS-F08            TO   PRT-EDENNO(X)
         MOVE  JIS-F09            TO   PRT-DATCNT(X)
         MOVE  JIS-F10            TO   PRT-DENCNT(X)
         PERFORM  SYUJISL1-READ-SEC
     END-PERFORM.
*印字する。
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FKZ00401"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    配送発注集計表出力
 SYUKEI010.
     WRITE    PRT-FKZ00401.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FKZ00401.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE               SYUJISL1  HTANMS  HTOKMS  ZSOKMS.
     CLOSE               PRTF.
*    終了メッセージ
     DISPLAY NC"＃手書出荷指示リスト出力＃" UPON CONS.
*
 END-EXT.
     EXIT.
****************************************************************
*    倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC      SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.

     READ   ZSOKMS
            INVALID     MOVE  "INV"   TO   ZSOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ          ＲＥＡＤ
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ   HTOKMS
            INVALID     MOVE  "INV"   TO   HTOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             担当者マスタ          ＲＥＡＤ
****************************************************************
 HTANMS-READ-SEC       SECTION.
*
     MOVE     "HTANMS-READ-SEC"   TO   S-NAME.
*
     READ   HTANMS
            INVALID     MOVE  "INV"   TO   HTANMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
