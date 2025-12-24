****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　発注業務　　　　　　　            *
*    モジュール名　　　　：　発注書商品明細ワーク作成　　　　　*
*    作成日／更新日　　　：　2015/05/13                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注書店舗ワークより、　　　　　  *
*                            発注書商品明細ワークデータを　　　*
*                            作成する。　　　　　　　          *
*    作成日／更新日　　　：　2015/08/12                        *
*                            発注書店舗ワークＬＦキー変更      *
*                            出荷日→店着日　　　　　　　      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3819B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/05/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*数量訂正ファイル
     SELECT   NFSUTEL1  ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01
                                            STE-F05
                                            STE-F06
                                            STE-F07
                                            STE-F08
                                            STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*発注書明細ワークＦ
     SELECT   HCSMXXX1  ASSIGN    TO        DA-01-VI-HCSMXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SME-F01
                                            SME-F05
                                            SME-F07
                                            SME-F09
                                            WITH DUPLICATES
                        FILE STATUS    IS   SME-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                        FILE STATUS    IS   SHO-STATUS.
*
*発注書店舗ワーク
     SELECT   HCTPXXX1  ASSIGN    TO        DA-01-VI-HCTPXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F01
                                            TEN-F05
                                            TEN-F09
                                            TEN-F06
                                            TEN-F07
                        FILE  STATUS   IS   TEN-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    数量訂正ファイル
******************************************************************
 FD  NFSUTEL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL1  OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    発注書明細ワークＦ
******************************************************************
 FD  HCSMXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HCSMXXX1  OF        XFDLIB
              JOINING   SME  AS   PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
******************************************************************
*    発注書店舗ワーク
******************************************************************
 FD  HCTPXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HCTPXXX1  OF        XFDLIB
              JOINING   TEN       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  KEP-FLG                 PIC  X(01)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(07)     VALUE  ZERO.
 01  NFSUTEL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFSUTEL1-SKIP-CNT       PIC  9(07)     VALUE  ZERO.
 01  SME-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  SME-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  TEN-DATA-CNT            PIC  9(07)     VALUE  ZERO.
 01  TEN-GYO-CNT             PIC  9(07)     VALUE  ZERO.
 01  TEN-WT-CNT              PIC  9(07)     VALUE  ZERO.
 01  TEN-REWT-CNT            PIC  9(07)     VALUE  ZERO.
 01  KIHON-ADD-CNT           PIC  9(07)     VALUE  ZERO.
 01  KIHON-UPD-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEN-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ZAMZAIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  HCTPXXX1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCTPXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCTPXXX1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFKOSUL1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WRK-AREA.
     03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
     03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*計算領域
 01  WRK-AREA3.
     03  WK-TORAY0.
       05  WK-TORAY          PIC 9(06)V9(02)   VALUE ZERO.
     03  WK-TORAYR           REDEFINES WK-TORAY0.
       05  WK-TORAY1         PIC 9(08).
     03  WK-KONPO0.
       05  WK-KONPO          PIC 9(06)V9(01)    VALUE ZERO.
     03  WK-KONPOR           REDEFINES WK-KONPO0.
       05  WK-KONPO1         PIC 9(07).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SME-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3819B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3819B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3819B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-KANRINO    PIC   9(08).
 01  PARA-IN-SAKUBACD   PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-KANRINO
                                       PARA-IN-SAKUBACD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL1.
     MOVE      "NFSUTEL1"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCSMXXX1.
     MOVE      "HCSMXXX1"   TO   AB-FILE.
     MOVE      SME-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCTPXXX1.
     MOVE      "HCTPXXX1"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     INITIALIZE                   WRK-AREA.
*
     OPEN     INPUT     NFSUTEL1  NFSHOMS1  HCTPXXX1.
     OPEN     I-O       HCSMXXX1.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
 INIT-01.
*数量訂正ファイルスタート
     PERFORM  NFSUTEL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　作成対象データ無１　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-02.
*数量訂正ファイル読込
     PERFORM NFSUTEL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　作成対象データ無２　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイルスタート
****************************************************************
 NFSUTEL1-START-SEC          SECTION.
*
     MOVE    "NFSUTEL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   STE-REC.
     INITIALIZE                        STE-REC.
*
     MOVE     PARA-IN-KANRINO     TO   STE-F01.
     MOVE     PARA-IN-SAKUBACD    TO   STE-F05.
*
     START  NFSUTEL1  KEY  IS  >= STE-F01
                                  STE-F05
                                  STE-F06
                                  STE-F07
                                  STE-F08
                                  STE-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 NFSUTEL1-START-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイル読込
****************************************************************
 NFSUTEL1-READ-SEC           SECTION.
*
     MOVE    "NFSUTEL1-READ-SEC"  TO   S-NAME.
*
     READ     NFSUTEL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-READ.
*管理番号チェック
     IF       STE-F01  NOT =  PARA-IN-KANRINO
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-IF.
*作場チェック
     IF       PARA-IN-SAKUBACD  NOT =  "  "
        IF    STE-F05  NOT =  PARA-IN-SAKUBACD
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-IF.
*
     ADD      1                   TO   NFSUTEL1-READ-CNT.
*
 NFSUTEL1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*発注書店舗ワーク存在チェック（ＴＢＬＮＯ.取得）
 MAIN-000.
     PERFORM HCTPXXX1-READ-SEC.
     IF      HCTPXXX1-INV-FLG = "INV"
             DISPLAY  NC"発注書店舗ワークなし！"   UPON  CONS
             DISPLAY  NC"　管理番号＝"  STE-F01    UPON  CONS
             DISPLAY  NC"　作場ＣＤ＝"  STE-F05    UPON  CONS
             DISPLAY  NC"　出荷日　＝"  STE-F14    UPON  CONS
             DISPLAY  NC"　店舗ＣＤ＝"  STE-F06    UPON  CONS
             DISPLAY  NC"　納品場所＝"  STE-F07    UPON  CONS
             MOVE    "END"       TO     END-FLG
             MOVE     4010       TO     PROGRAM-STATUS
             GO       TO         MAIN-EXIT
     END-IF.
*
*商品名称マスタ存在チェック（入数取得）
 MAIN-001.
     PERFORM NFSHOMS1-READ-SEC.
     IF      NFSHOMS1-INV-FLG = "INV"
             DISPLAY  NC"ナフコ商品マスタなし　商品Ｃ＝"
                      STE-F96     UPON  CONS
             ADD  1   TO         NFSUTEL1-SKIP-CNT
             GO       TO         MAIN-999
     END-IF.
     IF      SHO-F09          =  ZERO
             DISPLAY  NC"ナフコ商品マスタ入数ゼロ！　商品Ｃ＝"
                      STE-F96     UPON  CONS
             ADD  1   TO         NFSUTEL1-SKIP-CNT
             GO       TO         MAIN-999
     END-IF.
*トレー数算出
 MAIN-001.
     IF      NFSHOMS1-INV-FLG = "   "
             COMPUTE  WK-TORAY  =  STE-F11 / SHO-F09
             IF       WK-TORAY1(8:1)  NOT = 0
                      ADD   0.1    TO  WK-TORAY
             END-IF
     ELSE
             GO       TO           MAIN-999
     END-IF.
*
*明細ワークＦ存在チェック
 MAIN-020.
     PERFORM  HCSMXXX1-READ-SEC.
     IF  HCSMXXX1-INV-FLG = "INV"
         MOVE  SPACE        TO  SME-REC
         INITIALIZE             SME-REC
         MOVE  STE-F01      TO  SME-F01
         MOVE  STE-F02      TO  SME-F02
         MOVE  STE-F03      TO  SME-F03
         MOVE  STE-F04      TO  SME-F04
         MOVE  STE-F05      TO  SME-F05
         MOVE  STE-F14      TO  SME-F06
         MOVE  STE-F08      TO  SME-F07
         MOVE  STE-F13      TO  SME-F08
         MOVE  STE-F96      TO  SME-F09
         MOVE  SHO-F02      TO  SME-F10
         MOVE  SHO-F03      TO  SME-F11
         MOVE  SHO-F09      TO  SME-F12
         ADD   STE-F11      TO  SME-F201(TEN-F13)
         ADD   WK-TORAY     TO  SME-F202(TEN-F13)
         ADD   STE-F11      TO  SME-F21
         ADD   WK-TORAY     TO  SME-F22
*        合計箱数　
         COMPUTE  WK-KONPO  =  SME-F22  / 2
         IF       WK-KONPO1(7:1)  NOT = 0
                  ADD   1      TO  WK-KONPO
         END-IF
         MOVE  WK-KONPO     TO  SME-F23
*
         WRITE SME-REC
         ADD   1            TO  SME-ADD-CNT
     ELSE
         ADD   STE-F11      TO  SME-F201(TEN-F13)
         ADD   WK-TORAY     TO  SME-F202(TEN-F13)
         ADD   STE-F11      TO  SME-F21
         ADD   WK-TORAY     TO  SME-F22
*        合計箱数　
         COMPUTE  WK-KONPO  =  SME-F22  / 2
         IF       WK-KONPO1(7:1)  NOT = 0
                  ADD   1      TO  WK-KONPO
         END-IF
         MOVE  WK-KONPO     TO  SME-F23
*
         REWRITE SME-REC
         ADD   1            TO  SME-UPD-CNT
     END-IF.
*
 MAIN-999.
     PERFORM  NFSUTEL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    発注書店舗ワーク存在チェック（ＴＢＬＮＯ.取得）
****************************************************************
 HCTPXXX1-READ-SEC           SECTION.
*
     MOVE    "HCTPXXX1-READ-SEC"  TO   S-NAME.
*
     MOVE     STE-F01             TO   TEN-F01.     *>管理番号
     MOVE     STE-F05             TO   TEN-F05.     *>作場ＣＤ
     MOVE     STE-F08             TO   TEN-F09.     *>店着日
     MOVE     STE-F06             TO   TEN-F06.     *>店舗ＣＤ
     MOVE     STE-F07             TO   TEN-F07.     *>納品場所
     READ     HCTPXXX1
              INVALID     MOVE  "INV"  TO   HCTPXXX1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   HCTPXXX1-INV-FLG
     END-READ.
*
 HCTPXXX1-READ-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品マスタ読込（存在チェック）
****************************************************************
 NFSHOMS1-READ-SEC           SECTION.
*
     MOVE    "NFSHOMS1-READ-SEC"  TO   S-NAME.
*
     MOVE     STE-F96             TO   SHO-F01.     *>相手商品
     READ     NFSHOMS1
              INVALID     MOVE  "INV"  TO   NFSHOMS1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFSHOMS1-INV-FLG
     END-READ.
*
 NFSHOMS1-READ-EXIT.
     EXIT.
****************************************************************
*    箱数確定ファイル読込（存在チェック）
****************************************************************
 HCSMXXX1-READ-SEC           SECTION.
*
     MOVE    "HCSMXXX1-READ-SEC"  TO   S-NAME.
*
     MOVE     STE-F01             TO   SME-F01.   *>管理番号
     MOVE     STE-F05             TO   SME-F05.   *>作場ＣＤ
     MOVE     STE-F08             TO   SME-F07.   *>店着日
     MOVE     STE-F96             TO   SME-F09.   *>相手商品ＣＤ
     READ     HCSMXXX1
              INVALID     MOVE  "INV"  TO   HCSMXXX1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   HCSMXXX1-INV-FLG
     END-READ.
*
 HCSMXXX1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*数量訂正ファイル読込
     DISPLAY "NFSUTEL1  READ CNT = " NFSUTEL1-READ-CNT UPON CONS.
*ＳＫＩＰ
     DISPLAY "NFSUTEL1  SKIP CNT = " NFSUTEL1-SKIP-CNT UPON CONS.
*明細ワークＦ追加
     DISPLAY "HCSMXXX1  WRT  CNT = " SME-ADD-CNT      UPON CONS.
*明細ワークＦ更新
     DISPLAY "HCSMXXX1  UPD  CNT = " SME-UPD-CNT      UPON CONS.
*
     CLOSE     NFSUTEL1  NFSHOMS1
               HCTPXXX1  HCSMXXX1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
