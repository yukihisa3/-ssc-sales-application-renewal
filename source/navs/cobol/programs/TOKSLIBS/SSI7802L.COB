****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルHC 新EDIシステム
*    業務名　　　　　　　：　ロイヤルHC 新EDIシステム
*    モジュール名　　　　：　支払明細書発行指示                *
*    作成日／更新日　　　：　2008/11/21                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　8桁のパラメタ（支払締年月日）を
*                            受信後、明細を出力します。        *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI7802L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/11/21.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
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
*----<< 支払ヘッダデータ >>--*
     SELECT   ROYSHEPF  ASSIGN         DA-01-VI-ROYSHEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SIH-F03
                        STATUS         SIH-ST.
*----<< 支払明細データ >>--*
     SELECT   ROYSMEPF  ASSIGN         DA-01-VI-ROYSMEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIM-F01  SIM-F02
                                       SIM-F09  SIM-F04
                        STATUS         SIM-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払ヘッダファイル >>--*
 FD  ROYSHEPF           LABEL RECORD   IS   STANDARD.
     COPY     ROYSHEPF  OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 支払明細ファイル >>--*
 FD  ROYSMEPF           LABEL RECORD   IS   STANDARD.
     COPY     ROYSMEPF  OF        XFDLIB
              JOINING   SIM       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  READ-CNT       PIC  9(07).
 01  FLGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  SIH-FLG        PIC  X(01)     VALUE  SPACE.
     03  FIRST-FLG      PIC  X(01)     VALUE  SPACE.
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI7802L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIH-ST             PIC  X(02).
 01  SIM-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
 01  SIHARAI-JYOUHO.
     03  WK-TORICD      PIC  9(08)    VALUE  ZERO.
     03  WK-TENCD       PIC  9(08)    VALUE  ZERO.
     03  WK-TENCD1      PIC  9(08)    VALUE  ZERO.
     03  WK-SIHKBN      PIC  X(01)    VALUE  ZERO.
     03  WK-SHIGK-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-ZEIGK-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-SOSHI-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-SOZEI-KEI   PIC S9(09)    VALUE  ZERO.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TENNO  PIC  9(05).
     03  OLD.
         05  OLD-TENNO  PIC  9(05).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD000.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(09)     VALUE
                             NC"＜　支払明細書　＞".
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(20)     VALUE
                         NC"ロイヤルホームセンター".
         05  FILLER     PIC  X(68)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"作成日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-YYYY  PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD00-MM    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD00-DD    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  X(05)     VALUE  "PAGE:".
         05  HD00-PAGE  PIC  ZZ9.
 01  HDH1.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "==".
         05  FILLER     PIC  N(06)     VALUE  NC"＜支払情報＞".
         05  FILLER     PIC  X(50)     VALUE
          "==================================================".
         05  FILLER     PIC  X(50)     VALUE
          "==================================================".
         05  FILLER     PIC  X(24)     VALUE
          "=======================".
 01  HDH2.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(16)     VALUE
                           NC"≪　　日　　付　　情　　報　　≫".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(12)     VALUE
                                   NC"　≪　当月取引情報　≫　".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(17)     VALUE
                         NC"≪　　当　月　取　引　情　報　　≫".
 01  HDH3.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"締年月日：".
         05  HDH3-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH3-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH3-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品：".
         05  HDH3-NOHIN PIC  -,---,---,--9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(08)     VALUE
                             NC"買掛計上予定額：".
         05  HDH3-KAIYO PIC  --,---,---,--9.
 01  HDH4.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE NC"　支払年月日：".
         05  HDH4-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH4-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH4-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"値引：".
         05  HDH4-NEBIK PIC  -,---,---,--9.
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"調整額：".
         05  HDH4-CHOSE PIC  --,---,---,--9.
 01  HDH5.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"対象期間：".
         05  HDH5-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH5-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH5-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"値増：".
         05  HDH5-NEZOU PIC  -,---,---,--9.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"　相殺合計：".
         05  HDH5-SOSTA PIC  --,---,---,--9.
 01  HDH6.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"～".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HDH6-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH6-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  HDH6-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"返品：".
         05  HDH6-HENPI PIC  -,---,---,--9.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"　支払合計：".
         05  HDH6-SIHTA PIC  --,---,---,--9.
 01  HDH7.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(31)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"　税額：".
         05  HDH7-ZEIGA PIC  -,---,---,--9.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"　税額合計：".
         05  HDH7-ZEITA PIC  --,---,---,--9.
 01  HDM1.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
 01  HDM2.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE
                                        NC"納　品　日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE
                                        NC"検　収　日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(33)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票金額".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"税額".
*線１
 01  SEN01.
     03  FILLER         PIC  X(136)    VALUE  ALL "=".
*線２
 01  SEN02.
     03  FILLER         PIC  X(37)     VALUE  SPACE.
     03  FILLER         PIC  X(99)     VALUE  ALL "-".
*線３
 01  SEN03.
     03  FILLER         PIC  X(136)    VALUE  ALL "-".
*
 01  MS01.
     03  FILLER.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-TENCD PIC  ZZZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-TENNM PIC  X(28).
 01  MS02.
     03  FILLER         CHARACTER  TYPE  PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  MS21-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS21-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS21-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS22-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS22-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS22-DD    PIC  Z9.
*        05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS21-DENNO PIC  99999999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS21-DENKB PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS21-DENKM PIC  N(18).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS21-GENKI PIC  --,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS21-DENKI PIC  --,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS21-SHIKI PIC  --,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS21-ZEIKI PIC  --,---,---,--9.
 01  TA01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  X(50)     VALUE
          "==================================================".
         05  FILLER     PIC  X(37)     VALUE
          "=====================================".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"合　計　".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA01-SHITA PIC  -,---,---,--9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA01-ZEITA PIC  -,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(03)     VALUE  "===".
 01  TA000.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  X(50)     VALUE
          "==================================================".
         05  FILLER     PIC  X(37)     VALUE
          "=====================================".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"総合計　".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA00-SHITA PIC  -,---,---,--9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA00-ZEITA PIC  -,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(03)     VALUE  "===".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD8           PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-SIMEBI        PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION USING PARA-SIMEBI .
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払ヘッダデータ >>--*
 SIH-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ROYSHEPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  ROYSHEPF  ERROR " SIH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    ROYSHEPF.
     CLOSE    ROYSMEPF.
     STOP     RUN.
*----<< 支払明細データ >>--*
 SIM-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ROYSMEPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  ROYSMEPF  ERROR " SIM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    ROYSHEPF.
     CLOSE    ROYSMEPF.
     STOP     RUN.
*
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
*
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     ROYSHEPF.
     OPEN     INPUT     ROYSMEPF.
     OPEN     OUTPUT    PRTF.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
*    MOVE     LOW-VALUE      TO   BREAK-KEY.
*支払情報データ初期読込み
*締年月日(PARA)を元に支払ヘッダを読む
     MOVE     PARA-SIMEBI    TO   SIH-F03
     PERFORM  900-SIH-READ.
*締年月日を元に支払明細を読む
     MOVE     SIH-F03        TO   SIM-F01
     MOVE     ZERO  TO   SIM-F02  SIM-F09  SIM-F04
     PERFORM  900-SIM-START-READ
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
*店舗コードを退避します。
     MOVE     SIM-F02     TO  OLD-TENNO
     MOVE     "F"         TO  FIRST-FLG
     END-IF.

 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*明細
     PERFORM  MEISAI-WT-SEC.
*
*    店舗コードがブレイク
     IF       SIM-F02   NOT =  OLD-TENNO
              IF  PAGE-CNT > ZERO
                  PERFORM  TOTAL-WT-SEC
              END-IF
              MOVE      ZERO      TO  WK-SHIGK-KEI WK-ZEIGK-KEI
     END-IF.
     IF       NOT SIM-F02   =  OLD-TENNO
              WRITE    PRT-REC   FROM MS01    AFTER   2
              ADD      2         TO   LINE-CNT
     END-IF.
     IF           FIRST-FLG =  "F"
              WRITE    PRT-REC   FROM MS01    AFTER   2
         MOVE     "N"              TO  FIRST-FLG
              ADD      2         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC   FROM MS02    AFTER     1.
     ADD      1         TO   LINE-CNT.
*
     ADD      SIM-F12             TO  WK-SHIGK-KEI.
     ADD      SIM-F12             TO  WK-SOSHI-KEI.
     ADD      SIM-F13             TO  WK-ZEIGK-KEI.
     ADD      SIM-F13             TO  WK-SOZEI-KEI.
*
*店舗コードを退避します。
     MOVE     SIM-F02     TO  OLD-TENNO.
*
     PERFORM  900-SIM-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
*    総合計出力
     IF       PAGE-CNT       >    ZERO
              PERFORM   TOTAL-WT-SEC
              PERFORM   SOKEI-WT-SEC
     END-IF.
*ファイルのクローズ
     CLOSE    ROYSHEPF.
     CLOSE    ROYSMEPF.
     CLOSE    PRTF.
*
     DISPLAY "* SITGKFF (IN)=" READ-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     SPACE               TO   MS01.
*店舗情報取得
*店舗コード
     MOVE     SIM-F02             TO   MS01-TENCD
*店舗名
     MOVE     SIM-F03             TO   MS01-TENNM
*納品日
     MOVE     SIM-F07(1:4)        TO   MS21-YYYY.
     MOVE     SIM-F07(5:2)        TO   MS21-MM.
     MOVE     SIM-F07(7:2)        TO   MS21-DD.
*検収日
     MOVE     SIM-F09(1:4)        TO   MS22-YYYY.
     MOVE     SIM-F09(5:2)        TO   MS22-MM.
     MOVE     SIM-F09(7:2)        TO   MS22-DD.
*伝票番号
     MOVE     SIM-F04             TO   MS21-DENNO.
*伝票区分
     MOVE     SIM-F05             TO   MS21-DENKB.
     EVALUATE SIM-F05
         WHEN "01" MOVE NC"定番"               TO  MS21-DENKM
         WHEN "02" MOVE NC"特売"               TO  MS21-DENKM
         WHEN "03" MOVE NC"緊急"               TO  MS21-DENKM
         WHEN "04" MOVE NC"客注品"             TO  MS21-DENKM
         WHEN "06" MOVE NC"修理品"             TO  MS21-DENKM
         WHEN "08" MOVE NC"市場買"             TO  MS21-DENKM
         WHEN "10" MOVE NC"売上仕入"           TO  MS21-DENKM
         WHEN "13" MOVE NC"返品"               TO  MS21-DENKM
         WHEN "15" MOVE NC"原価修正"           TO  MS21-DENKM
         WHEN "80" MOVE NC"配送Ｃフィー"       TO  MS21-DENKM
         WHEN "81" MOVE NC"店直フィー"         TO  MS21-DENKM
         WHEN "82" MOVE NC"店直配送Ｃフィー"   TO  MS21-DENKM
         WHEN "83" MOVE NC"取引高歩引"         TO  MS21-DENKM
         WHEN "84" MOVE NC"情報フィー"         TO  MS21-DENKM
         WHEN "85" MOVE NC"物流改善フィー"     TO  MS21-DENKM
         WHEN "86" MOVE NC"支払決済歩引"       TO  MS21-DENKM
         WHEN "87" MOVE NC"新店オープンフィー" TO  MS21-DENKM
         WHEN "88" MOVE NC"会員セールフィー"   TO  MS21-DENKM
         WHEN "89" MOVE NC"欠品ペナルティ"     TO  MS21-DENKM
         WHEN "90" MOVE NC"リベート請求"       TO  MS21-DENKM
         WHEN "91" MOVE
                   NC"前月以前の返品・相殺過剰未収分の相殺"
                                                TO  MS21-DENKM
     END-EVALUATE.
*原価金額
     MOVE     SIM-F10             TO  MS21-GENKI.
*伝票金額
     MOVE     SIM-F11             TO  MS21-DENKI.
*支払金額
     MOVE     SIM-F12             TO  MS21-SHIKI.
*税額金額
     MOVE     SIM-F13             TO  MS21-ZEIKI.
*
 MEISAI-WT-EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      支払金額計・税額計出力                      *
*--------------------------------------------------------------*
 TOTAL-WT-SEC       SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-SHIGK-KEI   TO   TA01-SHITA.
     MOVE     WK-ZEIGK-KEI   TO   TA01-ZEITA.
     WRITE    PRT-REC        FROM TA01      AFTER     1.
     ADD      1              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-SHIGK-KEI.
     MOVE     ZERO           TO   WK-SHIGK-KEI.
*
 TOTAL-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5      総合計出力                                  *
*--------------------------------------------------------------*
 SOKEI-WT-SEC           SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-SOSHI-KEI   TO   TA00-SHITA.
     MOVE     WK-SOZEI-KEI   TO   TA00-ZEITA.
     WRITE    PRT-REC        FROM TA000     AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 SOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 HEAD-WT-SEC           SECTION.
*改頁制御
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*ページカウンター
     ADD      1                   TO   PAGE-CNT.
*行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*システム日付
     MOVE     SYS-YYW             TO   HD00-YYYY.
     MOVE     SYS-MMW             TO   HD00-MM.
     MOVE     SYS-DDW             TO   HD00-DD.
*頁
     MOVE     PAGE-CNT            TO   HD00-PAGE.
*
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HD000    AFTER     1.
     WRITE    PRT-REC   FROM      HD00     AFTER     1.
*１ページ目のみ支払情報を印字
     IF       PAGE-CNT  =    1
              PERFORM   SHIH-WT-SEC
     END-IF.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     WRITE    PRT-REC   FROM      HDM1      AFTER     1.
     WRITE    PRT-REC   FROM      HDM2      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     ADD      5         TO        LINE-CNT.
     MOVE     ZERO      TO        WK-TENCD1.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 5      支払情報印刷 ｼｮﾘ（１頁目のみ）
*--------------------------------------------------------------*
 SHIH-WT-SEC           SECTION.
*１ページ目のみ印字
*締年月日
     MOVE     SIH-F03(1:4)        TO   HDH3-YYYY.
     MOVE     SIH-F03(5:2)        TO   HDH3-MM.
     MOVE     SIH-F03(7:2)        TO   HDH3-DD.
*納品
     MOVE     SIH-F06             TO   HDH3-NOHIN.
*買掛計上予定額
     MOVE     SIH-F11             TO   HDH3-KAIYO.
*支払年月日
     MOVE     SIH-F04(1:4)        TO   HDH4-YYYY.
     MOVE     SIH-F04(5:2)        TO   HDH4-MM.
     MOVE     SIH-F04(7:2)        TO   HDH4-DD.
*値引
     MOVE     SIH-F07             TO   HDH4-NEBIK.
*調整額
     MOVE     SIH-F12             TO   HDH4-CHOSE.
*対象期間FROM
     MOVE     SIH-F01(1:4)        TO   HDH5-YYYY.
     MOVE     SIH-F01(5:2)        TO   HDH5-MM.
     MOVE     SIH-F01(7:2)        TO   HDH5-DD.
*値増
     MOVE     SIH-F08             TO   HDH5-NEZOU.
*相殺合計
     MOVE     SIH-F13             TO   HDH5-SOSTA.
*対象期間TO
     MOVE     SIH-F02(1:4)        TO   HDH6-YYYY.
     MOVE     SIH-F02(5:2)        TO   HDH6-MM.
     MOVE     SIH-F02(7:2)        TO   HDH6-DD.
*返品
     MOVE     SIH-F09             TO   HDH6-HENPI.
*支払合計
     MOVE     SIH-F14             TO   HDH6-SIHTA.
*税額
     MOVE     SIH-F10             TO   HDH7-ZEIGA.
*税額合計
     MOVE     SIH-F15             TO   HDH7-ZEITA.
*支払情報出力
     WRITE    PRT-REC   FROM      HDH1      AFTER     2.
     WRITE    PRT-REC   FROM      HDH2      AFTER     1.
     WRITE    PRT-REC   FROM      HDH3      AFTER     1.
     WRITE    PRT-REC   FROM      HDH4      AFTER     1.
     WRITE    PRT-REC   FROM      HDH5      AFTER     1.
     WRITE    PRT-REC   FROM      HDH6      AFTER     1.
     WRITE    PRT-REC   FROM      HDH7      AFTER     1.
     ADD      8         TO        LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払ヘッダデータ　　 REA                     *
*--------------------------------------------------------------*
 900-SIH-READ           SECTION.
     READ     ROYSHEPF  INVALID
              MOVE      "END"          TO   END-FLG
*             NOT  INVALID
*             MOVE      SPACE          TO   ROYSHEPF-INV-FLG
     END-READ.
 900-SIT-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　 START READ                *
*--------------------------------------------------------------*
 900-SIM-START-READ     SECTION.
     START    ROYSMEPF  KEY  >=   SIM-F01  SIM-F02
                                  SIM-F09  SIM-F04
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-SIM-START-READ-EXIT
     END-START.
*
     PERFORM   900-SIM-READ.
*
 900-SIM-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　       READ                *
*--------------------------------------------------------------*
 900-SIM-READ     SECTION.
     READ     ROYSMEPF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-SIM-READ-EXIT
     END-READ.
*
*締年月日チェック
          IF       SIM-F01   NOT =  PARA-SIMEBI
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-SIM-READ-EXIT
          END-IF
*
     ADD      1         TO        READ-CNT.
*
 900-SIM-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
