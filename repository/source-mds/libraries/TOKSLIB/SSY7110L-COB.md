# SSY7110L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY7110L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　仕入実績リスト出力                *
*    作成日／更新日　　　：　05/05/20                          *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　受信仕入実績ファイルを読み、仕入　*
*                            実績リストを発行する。            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7110L.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/20.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 受信仕入実績ファイル >>--*
     SELECT   HDJISKSF  ASSIGN         DA-01-S-HDJISKSF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         HDJISKSF-ST.
*----<< Ｊ本田出荷確定データ >>--*
*    SELECT   HDSYUKF   ASSIGN         DA-01-VI-HDSYUKL1
     SELECT   HDSYUKF   ASSIGN         DA-01-VI-HDSYUKL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
*                       RECORD    KEY  HDK-F01
*                                      HDK-F02
                        RECORD    KEY  HDK-F013
                                       HDK-F04
                                       HDK-F06
                        STATUS         HDSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 受信仕入実績ファイル >>--*
 FD  HDJISKSF           LABEL     RECORD   IS   STANDARD.
 01  HDJ-REC.
     03  HDJ-F011            PIC  X(02).
     03  HDJ-F012            PIC  X(02).
     03  HDJ-F013            PIC  9(05).
     03  HDJ-DATA            PIC  X(119).
     03  HDJ-H1        REDEFINES  HDJ-DATA.
         05  HDJ-H1-F02      PIC  X(02).
         05  HDJ-H1-F03      PIC  9(05).
         05  HDJ-H1-F04      PIC  X(01).
         05  HDJ-H1-F05      PIC  X(02).
         05  HDJ-H1-F06      PIC  X(04).
         05  HDJ-H1-F07      PIC  X(10).
         05  HDJ-H1-F08      PIC  X(04).
         05  HDJ-H1-F09      PIC  X(04).
         05  HDJ-H1-F10      PIC  X(01).
         05  HDJ-H1-F11      PIC  9(07).
         05  HDJ-H1-F12      PIC  9(07).
         05  HDJ-H1-F13      PIC S9(09)V9(02).
         05  HDJ-H1-F14      PIC S9(09)V9(02).
         05  HDJ-H1-F15      PIC  9(08).
         05  HDJ-H1-F16      PIC  X(42).
     03  HDJ-M1        REDEFINES  HDJ-DATA.
         05  HDJ-M1-F02      PIC  9(02).
         05  HDJ-M1-F03      PIC  9(02).
         05  HDJ-M1-F04      PIC  X(01).
         05  HDJ-M1-F05      PIC  X(13).
         05  HDJ-M1-F06      PIC S9(06)V9(02).
         05  HDJ-M1-F07      PIC S9(08)V9(02).
         05  HDJ-M1-F08      PIC S9(08)V9(02).
         05  HDJ-M1-F09      PIC  X(73).
*
*----<< Ｊ本田出荷確定データ >>--*
 FD  HDSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     HDSYUKF   OF        XFDLIB
              JOINING   HDK       PREFIX.
*
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC                 PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT            PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT            PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2           PIC  9(03)   VALUE  ZERO.
     03  READ-CNT            PIC  9(08)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG             PIC  X(03)   VALUE  SPACE.
     03  INV-FLG             PIC  X(03)   VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HDJISKSF-ST             PIC  X(02).
 01  HDSYUKF-ST              PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE                PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-DATEW               PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW             PIC  9(04).
     03  SYS-MMW             PIC  9(02).
     03  SYS-DDW             PIC  9(02).
 01  SYS-TIME                PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  SYS-MS              PIC  9(02).
*
 01  WK-SURYO                PIC  9(05)V9   VALUE  ZERO.
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-SURYO         PIC  9(09)V99.
     03  WK-GK-GENKA         PIC  9(09)V99.
     03  WK-GK-BAIKA         PIC  9(09).
*----<< 日付編集 ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY           PIC  9(04).
     03  WK-H-KU1            PIC  X(01).
     03  WK-H-MM             PIC  9(02).
     03  WK-H-KU2            PIC  X(01).
     03  WK-H-DD             PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-HDJ-F03          PIC  9(05).
     03  WK-HDJ-F12          PIC  9(07).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(113)    VALUE  SPACE.
         05  HD00-YDATE      PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MDATE      PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DDATE      PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD01.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER          PIC  X(51)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
             NC"□　仕入実績リスト　□".
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  X(03)     VALUE  "ｻｶﾀ".
         05  FILLER          PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  FILLER          PIC  N(03)     VALUE  NC"本田_".
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"店舗".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"部門".
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"検収日".
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"特売区分".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"訂正".
         05  FILLER          PIC  X(03)     VALUE  "FLG".
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(13)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"行".
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"商品".
         05  FILLER          PIC  X(02)     VALUE  "CD".
         05  FILLER          PIC  X(09)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"商品名".
         05  FILLER          PIC  X(20)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"規格名".
         05  FILLER          PIC  X(13)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"入荷数".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"原単価".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"原価金額".
 01  SEN                CHARACTER  TYPE  MODE-2.
     03  FILLER              PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER              PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER              PIC  N(18)  VALUE
         NC"──────────────────".
*
 01  MS01.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(09).
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS01-JDENNO     PIC  X(07).
         05  FILLER          PIC  X(02)     VALUE  ") ".
         05  MS01-TENCD      PIC  X(04).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TENNM      PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-BUMON      PIC  X(04).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-KENDT      PIC  X(10).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-TOKKB      PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  MS01-TOKKBN     PIC  N(02).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-TEIFLG     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  MS01-TEIFNM     PIC  N(06).
 01  MS02.
     03  FILLER.
         05  FILLER          PIC  X(13)     VALUE  SPACE.
         05  MS02-GYO        PIC  Z9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-SYOKB      PIC  X(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS02-SYOCD      PIC  X(13).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-SYONM      PIC  X(25).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-KIKAKU     PIC  X(15).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS02-SURYO      PIC  ---,--9.99.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-GENTAN     PIC  ---,--9.99.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-GENKIN     PIC  ---,---,--9.
 01  MS03.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(64)    VALUE  SPACE.
         05  FILLER          PIC  N(05)    VALUE
             NC"□合　計□".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  MS03-SUGOK      PIC  ---,---,--9.
         05  FILLER          PIC  X(15)    VALUE  SPACE.
         05  MS03-GENGOK     PIC  ---,---,--9.
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
*LINKAGE     SECTION.
*01  LINK-JDATE             PIC  9(08).
*01  LINK-JTIME             PIC  9(04).
*01  LINK-TORICD            PIC  9(08).
*01  LINK-SOKO              PIC  X(02).
*01  LINK-NOUDT             PIC  9(08).
*01  LINK-DSOKO             PIC  X(02).
****************************************************************
 PROCEDURE              DIVISION.
*PROCEDURE              DIVISION  USING  LINK-JDATE
*                                        LINK-JTIME
*                                        LINK-TORICD
*                                        LINK-SOKO
*                                        LINK-NOUDT
*                                        LINK-DSOKO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 受信仕入実績ファイル >>--*
 HDJISKSF-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDJISKSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7110L HDSYUKF ERROR " HDJISKSF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HDJISKSF  HDSYUKF.
     STOP     RUN.
*
*----<< Ｊ本田出荷確定データ >>--*
 HDSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7110L HDSYUKF ERROR " HDSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HDJISKSF  HDSYUKF.
     STOP     RUN.
*
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
     DISPLAY  "*** SSY7110L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HDJISKSF  HDSYUKF.
     OPEN     OUTPUT    PRTF.
*
*    受信仕入実績ファイルＲＥＡＤ
     PERFORM  900-HDJ-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT
                                       PAGE-CNT2
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    レコード区分＝‘Ｈ１’のとき，明細ヘッダセット
     IF       HDJ-F012   =  "H1"
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
*
              MOVE      HDJ-H1-F03     TO   WK-HDJ-F03
              MOVE      HDJ-H1-F12     TO   WK-HDJ-F12
              MOVE      ZERO           TO   GOKEI-AREA
     END-IF.
*    レコード区分＝‘Ｍ１’のとき，明細行セット
     IF       HDJ-F012   =  "M1"
              PERFORM   MEISAI-BODY-SEC
     END-IF.
*    受信仕入実績ファイルＲＥＡＤ
     PERFORM  900-HDJ-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     DISPLAY "対象DT CNT = " READ-CNT  UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
     END-IF.
*
     CLOSE    HDJISKSF  HDSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7110L END *** "
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
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT  PAGE-CNT2.
     MOVE     PAGE-CNT2           TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YDATE.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MDATE.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DDATE.
*
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  3.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  1.
*行カウント
     MOVE     9                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ヘッダー印字
*--------------------------------------------------------------*
 MEISAI-HEAD-SEC       SECTION.
     MOVE     "MEISAI-HEAD-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  50
            PERFORM   HEAD-WT-SEC
     END-IF.
*    サカタ伝票番号
     MOVE     HDJ-H1-F12          TO   MS01-DENNO.
*    Ｊ本田伝票番号
     MOVE     HDJ-H1-F11          TO   MS01-JDENNO.
*    店舗ＣＤ
     MOVE     HDJ-H1-F06          TO   MS01-TENCD.
*    店舗名称
     MOVE     HDJ-H1-F07          TO   MS01-TENNM.
*    部門
     MOVE     HDJ-H1-F08          TO   MS01-BUMON.
*    検収日
     MOVE     HDJ-H1-F15(1:4)     TO   WK-H-YYYY.
     MOVE     HDJ-H1-F15(5:2)     TO   WK-H-MM.
     MOVE     HDJ-H1-F15(7:2)     TO   WK-H-DD.
     MOVE     "/"                 TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU    TO   MS01-KENDT.
*    特売区分
     MOVE     HDJ-H1-F10          TO   MS01-TOKKB.
*    特売区分名
     MOVE     SPACE               TO   MS01-TOKKBN.
     IF       HDJ-H1-F10   =   "1"
              MOVE    NC"定番"    TO   MS01-TOKKBN
     END-IF.
     IF       HDJ-H1-F10   =   "9"
              MOVE    NC"特売"    TO   MS01-TOKKBN
     END-IF.
*    訂正ＦＬＧ
     MOVE     HDJ-H1-F02          TO   MS01-TEIFLG.
*    訂正ＦＬＧ名
     MOVE     SPACE               TO   MS01-TEIFNM.
     IF       HDJ-H1-F02   =   "00"
              MOVE    NC"新規データ"   TO   MS01-TEIFNM
     END-IF.
     IF       HDJ-H1-F02   =   "01"
              MOVE    NC"修正マイナス" TO   MS01-TEIFNM
     END-IF.
     IF       HDJ-H1-F02   =   "02"
              MOVE    NC"修正プラス"   TO   MS01-TEIFNM
     END-IF.
*
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*
     ADD      1                   TO   LINE-CNT.
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
     MOVE     HDJ-M1-F03          TO   MS02-GYO.
*    商品ＣＤ区分
     MOVE     HDJ-M1-F04          TO   MS02-SYOKB.
*    商品ＣＤ
     MOVE     HDJ-M1-F05          TO   MS02-SYOCD.
*
*    Ｊ本田出荷確定データＲＥＡＤ
     PERFORM  900-HDK-READ.
     IF       INV-FLG   =   SPACE
*             品名
              MOVE     HDK-A27    TO   MS02-SYONM
*             規格
              MOVE     HDK-A28    TO   MS02-KIKAKU
     ELSE
              MOVE     SPACE      TO   MS02-SYONM
                                       MS02-KIKAKU
     END-IF.
*    入荷数
     MOVE     HDJ-M1-F06          TO   MS02-SURYO  WK-SURYO.
     COMPUTE  WK-GK-SURYO  =  WK-GK-SURYO + HDJ-M1-F06.
*    原価
     MOVE     HDJ-M1-F07          TO   MS02-GENTAN.
*    原価金額
     COMPUTE  MS02-GENKIN  =  WK-SURYO     *  HDJ-M1-F07.
     COMPUTE  WK-GK-GENKA  =  WK-GK-GENKA  +
                            ( WK-SURYO     *  HDJ-M1-F07).
*
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
*
     ADD      1                   TO   LINE-CNT.
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC"      TO   S-NAME.
*    数量合計
     MOVE     WK-GK-SURYO         TO   MS03-SUGOK.
*    原価金額合計
     MOVE     WK-GK-GENKA         TO   MS03-GENGOK.
*
     WRITE    PRT-REC       FROM  MS03  AFTER  1.
     MOVE     SPACE               TO   PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*
     ADD      2                   TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受信仕入実績ファイル   READ                  *
*--------------------------------------------------------------*
 900-HDJ-READ           SECTION.
     MOVE     "900-HDJ-READ"      TO   S-NAME.
*
     READ     HDJISKSF
         AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-HDJ-READ-EXIT
     END-READ.
*ファイルヘッダーは読飛し
     IF       HDJ-F012   =   "F1"
              GO                  TO   900-HDJ-READ
     END-IF.
*
     ADD      1                   TO   READ-CNT.
*
 900-HDJ-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    Ｊ本田出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-HDK-READ           SECTION.
     MOVE     "900-HDK-READ"      TO   S-NAME.
*
     MOVE     SPACE               TO   INV-FLG.
     MOVE     WK-HDJ-F03          TO   HDK-F013.
     MOVE     WK-HDJ-F12          TO   HDK-F04.
     MOVE     HDJ-M1-F03          TO   HDK-F06.
*
     READ     HDSYUKF
         INVALID
              MOVE     "INV"      TO   INV-FLG
     END-READ.
*
 900-HDK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
