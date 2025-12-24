****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　カーマオンラインシステム　　　　　*
*    モジュール名　　　　：　出荷リスト出力                    *
*    作成日／更新日　　　：　02/04/05                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　カーマ出荷確定データを読み、出荷　*
*                            リストを発行する。                *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3110L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/04/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< カーマ出荷確定データ >>--*
     SELECT   KMSYUKF   ASSIGN         DA-01-VI-KMSYUKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KMK-F01
                                       KMK-F02
                                       KMK-F03
                                       KMK-F04
                                       KMK-F05
                                       KMK-F09A
                                       KMK-F09B
                                       KMK-F06
                                       KMK-F07
                        STATUS         KMSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< カーマ出荷確定データ >>--*
 FD  KMSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     KMSYUKF   OF        XFDLIB
              JOINING   KMK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(08)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG        PIC  X(03)   VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KMSYUKF-ST        PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  WK-SURYO           PIC  9(05)V9   VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(09)V99.
     03  WK-GK-BAIKA    PIC  9(09).
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-KMK-F01     PIC  9(08).
     03  WK-KMK-F02     PIC  9(04).
     03  WK-KMK-F03     PIC  9(08).
     03  WK-KMK-F04     PIC  X(02).
     03  WK-KMK-F05     PIC  9(05).
     03  WK-KMK-F09A    PIC  9(08).
     03  WK-KMK-F09B    PIC  9(08).
     03  WK-KMK-F06     PIC  9(09).
     03  WK-KMK-F07     PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(114)    VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"日付".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-SDATE PIC  X(10).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-PCNT  PIC  ZZ9.
 01  HD001.
     03  FILLER         PIC  X(104)    VALUE  SPACE.
     03  FILLER         PIC  X(31)     VALUE
        "-------------------------------".
 01  HD002.
     03  FILLER         PIC  X(104)    VALUE  SPACE.
     03  FILLER         PIC  X(31)     VALUE
        "!               !             !".
 01  HD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
             NC"株式会社カーマ".
         05  FILLER     PIC  X(32)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-2-5.
         05  FILLER     PIC  N(05)     VALUE
             NC"出荷リスト".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  FILLER     PIC  X(04)     VALUE  "!   ".
         05  FILLER     PIC  N(02)     VALUE
             NC"検品".
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  FILLER     PIC  N(03)     VALUE
             NC"検品者".
         05  FILLER     PIC  X(09)     VALUE  "   !     ".
         05  FILLER     PIC  N(03)     VALUE
             NC"検収印".
         05  FILLER     PIC  X(05)     VALUE  "    !".
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(10)     VALUE  "ﾍﾞﾝﾀﾞｰｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(06)     VALUE  "ﾍﾞﾝﾀﾞｰ".
         05  FILLER     PIC  N(02)     VALUE  NC"名称".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"店".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"館名".
         05  FILLER     PIC  X(59)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷日".
 01  HD03.
     03  FILLER.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TOKCD PIC  99999.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  HD03-JISYA PIC  X(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TENCD PIC  999.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD03-TENNM PIC  X(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-KANNM PIC  X(10).
         05  FILLER     PIC  X(49)     VALUE  SPACE.
         05  HD03-HACD  PIC  X(10).
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  HD03-NOUD  PIC  X(10).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品番号".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"品名".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"規格".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"特売".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  N(03)     VALUE  NC"申込表".
         05  FILLER     PIC  X(03)     VALUE  "NO.".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"引合".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"原価".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"売価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
 01  SEN                         CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
*
 01  MS01.
     03  FILLER                  CHARACTER  TYPE  MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  999999.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-BUMON PIC  99.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  MS01-DENK  PIC  99.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備考：".
         05  MS01-BIKO  PIC  X(20).
 01  MS02.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-GYO   PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYOCD PIC  9999999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-JANCD PIC  X(13).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYONM1 PIC  X(20).
*********05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYONM2 PIC  X(20).
*********05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-TOK   PIC  X(06).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-MOS   PIC  X(06).
*********05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SURYO PIC  ---,--9.9.
         05  FILLER     PIC  X(06)     VALUE  "(    )".
         05  MS02-GENTAN PIC  -,---,--9.99.
         05  MS02-BAITAN PIC  -,---,--9.
         05  MS02-GENKIN PIC  -,---,--9.99.
         05  MS02-BAIKIN PIC  -,---,--9.
 01  MS03.
     03  FILLER.
         05  FILLER     PIC  X(115)    VALUE  SPACE.
         05  MS03-GENGOK PIC  -,---,--9.99.
         05  MS03-BAIGOK PIC  -,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "-".
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カーマ出荷確定データ >>--*
 KMSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KMSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3110L KMSYUKF ERROR " KMSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    KMSYUKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     DISPLAY  "*** SSY3110L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     KMSYUKF.
     OPEN     OUTPUT    PRTF.
*
*カーマ出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   KMK-REC.
     INITIALIZE                        KMK-REC.
     MOVE     LINK-JDATE          TO   KMK-F01.
     MOVE     LINK-JTIME          TO   KMK-F02.
     MOVE     LINK-TORICD         TO   KMK-F03.
     MOVE     LINK-SOKO           TO   KMK-F04.
     MOVE     LINK-STENCD         TO   KMK-F05.
     PERFORM  900-KMK-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     KMK-F01    TO   WK-KMK-F01
              MOVE     KMK-F02    TO   WK-KMK-F02
              MOVE     KMK-F03    TO   WK-KMK-F03
              MOVE     KMK-F04    TO   WK-KMK-F04
              MOVE     KMK-F05    TO   WK-KMK-F05
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F09B   TO   WK-KMK-F09B
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    店舗ＣＤ／発注日／納品日がブレイク時
     IF       KMK-F05   NOT =  WK-KMK-F05
     OR       KMK-F09A  NOT =  WK-KMK-F09A
     OR       KMK-F09B  NOT =  WK-KMK-F09B
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              PERFORM   MEISAI-HEAD-SEC
              MOVE      KMK-F05   TO  WK-KMK-F05
              MOVE      KMK-F09A  TO  WK-KMK-F09A
              MOVE      KMK-F09B  TO  WK-KMK-F09B
              MOVE      KMK-F06   TO  WK-KMK-F06
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       KMK-F06   NOT =  WK-KMK-F06
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
              MOVE      KMK-F06   TO  WK-KMK-F06
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    カーマ出荷確定データ読込み
     PERFORM  900-KMK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
     END-IF.
*
     CLOSE    KMSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY3110L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*    頁カウンター
     ADD      1         TO        PAGE-CNT  PAGE-CNT2.
     MOVE     PAGE-CNT2 TO        HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD00-SDATE.
*    ベンダーＣＤ
     MOVE     KMK-F093       TO   HD03-TOKCD.
*    ベンダー名称
     MOVE     KMK-F09C       TO   HD03-JISYA.
*    店ＣＤ
     MOVE     KMK-F095       TO   HD03-TENCD.
*    店舗名称
     MOVE     KMK-F09E       TO   HD03-TENNM.
*    館名称
     MOVE     "ｶﾞｰﾃﾞﾝｾﾝﾀｰ"   TO   HD03-KANNM.
*    発注日
     MOVE     KMK-F09A(1:4)  TO   WK-H-YYYY.
     MOVE     KMK-F09A(5:2)  TO   WK-H-MM.
     MOVE     KMK-F09A(7:2)  TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-HACD.
*    出荷日
     MOVE     KMK-F14(1:4)   TO   WK-H-YYYY.
     MOVE     KMK-F14(5:2)   TO   WK-H-MM.
     MOVE     KMK-F14(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-NOUD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD00  AFTER  0.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  2.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  1.
*行カウント
     MOVE     12             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ヘッダー印字
*--------------------------------------------------------------*
 MEISAI-HEAD-SEC       SECTION.
     MOVE     "MEISAI-HEAD-SEC"    TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  50
            PERFORM   HEAD-WT-SEC
     END-IF.
*    納品番号（伝票番号）
     MOVE      KMK-F096    TO   MS01-DENNO.
*    部門
     MOVE      KMK-F098    TO   MS01-BUMON.
*    伝区
     MOVE      KMK-F099    TO   MS01-DENK.
*    備考
     MOVE      SPACE       TO   MS01-BIKO.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
*
     ADD       1           TO   LINE-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  50
            PERFORM   HEAD-WT-SEC
     END-IF.
*    行
     MOVE   KMK-F07               TO   MS02-GYO.
*    商品ＣＤ
     MOVE   KMK-F106              TO   MS02-SYOCD.
*    ＪＡＮＣＤ
     MOVE   KMK-F10A              TO   MS02-JANCD.
*    品名
     MOVE   KMK-F108              TO   MS02-SYONM1.
*    規格
     MOVE   KMK-F109              TO   MS02-SYONM2.
*    特売ＣＤ
     MOVE   KMK-F09K              TO   MS02-TOK.
*    申込票
     MOVE   SPACE                 TO   MS02-MOS.
*    出荷数
     IF     KMK-F15  >  ZERO
            COMPUTE  MS02-SURYO  =  KMK-F15  /  10
            COMPUTE  WK-SURYO    =  KMK-F15  /  10
     ELSE
            MOVE     ZERO         TO   MS02-SURYO  WK-SURYO
     END-IF.
*    引合
*    原価
     COMPUTE  MS02-GENTAN  =  KMK-F10C  /  100.
*    売価
     MOVE   KMK-F10D              TO   MS02-BAITAN.
*    原価金額
     COMPUTE  MS02-GENKIN  =  ( WK-SURYO * KMK-F10C )  /  100.
     COMPUTE  WK-GK-GENKA  =  WK-GK-GENKA +
                              ( WK-SURYO * KMK-F10C )  /  100.
*    売価金額
     COMPUTE  MS02-BAIKIN  =  ( WK-SURYO * KMK-F10D ).
     COMPUTE  WK-GK-BAIKA  =  WK-GK-BAIKA +
                              ( WK-SURYO * KMK-F10D ).
*    〇以上の場合に出力対象
     IF   WK-SURYO  >  ZERO
          WRITE     PRT-REC   FROM  MS02  AFTER  1
          ADD       1           TO   LINE-CNT
     END-IF.
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC" TO   S-NAME.
*    原価金額合計
     MOVE     WK-GK-GENKA       TO    MS03-GENGOK.
*    売価金額合計
     MOVE     WK-GK-BAIKA       TO    MS03-BAIGOK.
*
     WRITE     PRT-REC   FROM  MS03  AFTER  1.
     WRITE     PRT-REC   FROM  SEN1  AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       2           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-KMK-START-READ     SECTION.
     MOVE     "900-KMK-START-READ"     TO   S-NAME.
     START    KMSYUKF   KEY  >=   KMK-F01  KMK-F02
                                  KMK-F03  KMK-F04
                                  KMK-F05  KMK-F09A
                                  KMK-F09B KMK-F06
                                  KMK-F07
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-KMK-START-READ-EXIT
     END-START.
*
     PERFORM   900-KMK-READ.
*
 900-KMK-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    カーマ出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-KMK-READ           SECTION.
     MOVE     "900-KMK-READ"      TO   S-NAME.
*
     READ     KMSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-KMK-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF       KMK-F01   NOT =  LINK-JDATE       OR
              KMK-F02   NOT =  LINK-JTIME       OR
              KMK-F03   NOT =  LINK-TORICD      OR
              KMK-F04   NOT =  LINK-SOKO
              MOVE     "END"      TO   END-FLG
              GO        TO        900-KMK-READ-EXIT
     END-IF.
*店舗ＣＤ範囲チェック
***  DISPLAY "LINK-STENCD = " LINK-STENCD  UPON CONS.
***  DISPLAY "LINK-ETENCD = " LINK-ETENCD  UPON CONS.
***  DISPLAY "KMK-F05     = " KMK-F05      UPON CONS.
     IF       LINK-STENCD  <=  KMK-F05
     AND      LINK-ETENCD  >=  KMK-F05
              CONTINUE
     ELSE
              GO        TO        900-KMK-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-KMK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
