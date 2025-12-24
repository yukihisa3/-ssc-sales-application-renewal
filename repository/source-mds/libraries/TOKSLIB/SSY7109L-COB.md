# SSY7109L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY7109L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　出荷確定リスト出力                *
*    作成日／更新日　　　：　05/05/19                          *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　Ｊ本田出荷確定データを読み、出荷　*
*                            確定リストを発行する。            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7109L.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/19.
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
*----<< Ｊ本田出荷確定データ >>--*
     SELECT   HDSYUKF   ASSIGN         DA-01-VI-HDSYUKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HDK-F01
                                       HDK-F02
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
*----<< Ｊ本田出荷確定データ >>--*
 FD  HDSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     HDSYUKF   OF        XFDLIB
              JOINING   HDK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
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
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
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
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-SURYO         PIC  9(09)V99.
     03  WK-GK-GENKA         PIC  9(09)V99.
     03  WK-GK-BAIKA         PIC  9(09).
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY           PIC  9(04).
     03  WK-H-KU1            PIC  X(01).
     03  WK-H-MM             PIC  9(02).
     03  WK-H-KU2            PIC  X(01).
     03  WK-H-DD             PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-HDK-F011         PIC  9(08).
     03  WK-HDK-F012         PIC  9(04).
     03  WK-HDK-F013         PIC  9(08).
     03  WK-HDK-F02          PIC  X(02).
     03  WK-HDK-F04          PIC  9(09).
     03  WK-HDK-F06          PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
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
*    03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER          PIC  X(51)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
             NC"□　出荷確定リスト　□".
 01  HD02.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"バッチ_：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD02-JDATE      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD02-JTIME      PIC  9(04).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD02-TORICD     PIC  9(08).
 01  HD03.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"倉庫：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD03-SOKO       PIC  X(02).
 01  HD04.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
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
         05  FILLER          PIC  N(03)     VALUE  NC"発注日".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品日".
 01  HD05.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
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
         05  FILLER          PIC  N(03)     VALUE  NC"数　量".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"原単価".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"売単価".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"売価金額".
 01  SEN                CHARACTER  TYPE  MODE-2.
     03  FILLER              PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER              PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER              PIC  N(18)  VALUE
         NC"──────────────────".
*
 01  MS01.
     03  FILLER.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(09).
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS01-JDENNO     PIC  X(07).
         05  FILLER          PIC  X(02)     VALUE  ") ".
         05  MS01-TENCD      PIC  9(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TENNM      PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-BUMON      PIC  X(04).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-HATDT      PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-NOUDT      PIC  X(10).
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
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-BAITAN     PIC  ---,--9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-BAIKIN     PIC  ---,---,--9.
 01  MS03.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER          PIC  X(64)    VALUE  SPACE.
         05  FILLER          PIC  N(05)    VALUE
             NC"□合　計□".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  MS03-SUGOK      PIC  ---,---,--9.
         05  FILLER          PIC  X(15)    VALUE  SPACE.
         05  MS03-GENGOK     PIC  ---,---,--9.
         05  FILLER          PIC  X(09)    VALUE  SPACE.
         05  MS03-BAIGOK     PIC  ---,---,--9.
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
 01  LINK-NOUDT             PIC  9(08).
 01  LINK-DSOKO             PIC  X(02).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-NOUDT
                                         LINK-DSOKO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< Ｊ本田出荷確定データ >>--*
 HDSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7109L HDSYUKF ERROR " HDSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HDSYUKF.
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
     DISPLAY  "*** SSY7109L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HDSYUKF.
     OPEN     OUTPUT    PRTF.
*
*Ｊ本田出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   HDK-REC.
     INITIALIZE                        HDK-REC.
     MOVE     LINK-JDATE          TO   HDK-F011.
     MOVE     LINK-JTIME          TO   HDK-F012.
     MOVE     LINK-TORICD         TO   HDK-F013.
     MOVE     LINK-SOKO           TO   HDK-F02.
     MOVE     ZERO                TO   HDK-F04
                                       HDK-F06.
     PERFORM  900-HDK-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     HDK-F011   TO   WK-HDK-F011
              MOVE     HDK-F012   TO   WK-HDK-F012
              MOVE     HDK-F013   TO   WK-HDK-F013
              MOVE     HDK-F02    TO   WK-HDK-F02
              MOVE     ZERO       TO   WK-HDK-F04
                                       WK-HDK-F06
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
*    倉庫ＣＤがブレイク時
     IF       HDK-F02   NOT =  WK-HDK-F02
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              PERFORM   MEISAI-HEAD-SEC
              MOVE      HDK-F02   TO  WK-HDK-F02
              MOVE      HDK-F04   TO  WK-HDK-F04
              MOVE      ZERO      TO  GOKEI-AREA
     END-IF.
*    伝票番号がブレイク
     IF       HDK-F04   NOT =  WK-HDK-F04
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
              MOVE      HDK-F04   TO  WK-HDK-F04
              MOVE      ZERO      TO  GOKEI-AREA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    Ｊ本田出荷確定データ読込み
     PERFORM  900-HDK-READ.
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
     CLOSE    HDSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7109L END *** "
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
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YDATE.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MDATE.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DDATE.
*    受信日
     MOVE     HDK-F011            TO   HD02-JDATE.
*    受信時間
     MOVE     HDK-F012            TO   HD02-JTIME.
*    取引先ＣＤ
     MOVE     HDK-F013            TO   HD02-TORICD.
*    倉庫ＣＤ
     MOVE     HDK-F02             TO   HD03-SOKO.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
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
     MOVE     HDK-F04             TO   MS01-DENNO.
*    Ｊ本田伝票番号
     MOVE     HDK-F05             TO   MS01-JDENNO.
*    店舗ＣＤ
     MOVE     HDK-F03             TO   MS01-TENCD.
*    店舗名称
     MOVE     HDK-A10             TO   MS01-TENNM.
*    部門
     MOVE     HDK-A14             TO   MS01-BUMON.
*    発注日
     MOVE     HDK-A16(1:4)        TO   WK-H-YYYY.
     MOVE     HDK-A16(5:2)        TO   WK-H-MM.
     MOVE     HDK-A16(7:2)        TO   WK-H-DD.
     MOVE     "/"                 TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU    TO   MS01-HATDT.
*    納品日
     MOVE     HDK-F07(1:4)        TO   WK-H-YYYY.
     MOVE     HDK-F07(5:2)        TO   WK-H-MM.
     MOVE     HDK-F07(7:2)        TO   WK-H-DD.
     MOVE     "/"                 TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU    TO   MS01-NOUDT.
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
     MOVE     HDK-F06             TO   MS02-GYO.
*    商品ＣＤ区分
     MOVE     HDK-A25             TO   MS02-SYOKB.
*    商品ＣＤ
     MOVE     HDK-A26             TO   MS02-SYOCD.
*    品名
     MOVE     HDK-A27             TO   MS02-SYONM.
*    規格
     MOVE     HDK-A28             TO   MS02-KIKAKU.
*    出荷数
     MOVE     HDK-F10             TO   MS02-SURYO  WK-SURYO.
     COMPUTE  WK-GK-SURYO  =  WK-GK-SURYO + HDK-F10.
*    原価
     MOVE     HDK-A32             TO   MS02-GENTAN.
*    売価
     MOVE     HDK-A33             TO   MS02-BAITAN.
*    原価金額
     COMPUTE  MS02-GENKIN  =  WK-SURYO     *  HDK-A32.
     COMPUTE  WK-GK-GENKA  =  WK-GK-GENKA  +
                            ( WK-SURYO     *  HDK-A32 ).
*    売価金額
     COMPUTE  MS02-BAIKIN  =  WK-SURYO     *  HDK-A33.
     COMPUTE  WK-GK-BAIKA  =  WK-GK-BAIKA  +
                            ( WK-SURYO     *  HDK-A33 ).
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
*    売価金額合計
     MOVE     WK-GK-BAIKA         TO   MS03-BAIGOK.
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
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-HDK-START-READ     SECTION.
     MOVE     "900-HDK-START-READ"     TO   S-NAME.
     START    HDSYUKF   KEY  >=   HDK-F01  HDK-F02
                                  HDK-F04  HDK-F06
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-HDK-START-READ-EXIT
     END-START.
*
     PERFORM   900-HDK-READ.
*
 900-HDK-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    Ｊ本田出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-HDK-READ           SECTION.
     MOVE     "900-HDK-READ"      TO   S-NAME.
*
     READ     HDSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-HDK-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF       HDK-F011   NOT =  LINK-JDATE       OR
              HDK-F012   NOT =  LINK-JTIME       OR
              HDK-F013   NOT =  LINK-TORICD
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-HDK-READ-EXIT
     END-IF.
     IF       LINK-SOKO  NOT =  SPACE            AND
              HDK-F02    NOT =  LINK-SOKO
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-HDK-READ-EXIT
     END-IF.
*    納品日チェック
     IF       LINK-NOUDT  NOT =  ZERO
              IF       HDK-F07    NOT =  LINK-NOUDT
                       GO         TO   900-HDK-READ
              END-IF
     END-IF.
*
     ADD      1                   TO   READ-CNT.
*
 900-HDK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
