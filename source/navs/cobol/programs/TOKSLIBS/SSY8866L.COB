****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　オンライン　　　*
*    モジュール名　　　　：　欠品明細書発行                    *
*    作成日／更新日　　　：　14/08/21                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　欠品明細データを読み、欠品明細書　*
*                            を発行する。　　　　　　　　　　　*
*    作成日／更新日　　　：　17/03/21                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　カーマ東日本出荷対応　　　　　　　*
**履歴**********************************************************
*    2017/03/21  高橋　　カーマ東日本納品対応　　　　　　　　　*
*    2018/03/19　高橋　　ケーヨー追加　　　　　　　　　　　　　*
*    2019/03/19　高橋　　発注種別変換マスタ対応　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY8866L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/08/20.
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
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＤＣＭ出荷確定データ >>--*
     SELECT   KPSYUKF   ASSIGN         DA-01-VI-KPSYUKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KMK-K01
                                       KMK-K02
                                       KMK-K03
                                       KMK-K04
                                       KMK-F09
                                       KMK-F05
                                       KMK-K08
                                       KMK-F21
                                       KMK-M091
                                       KMK-F03
                                       KMK-M03
                        STATUS         KPSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*#2019/03/19 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL1  ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   DCMHSBL1-ST.
*#2019/03/19 NAV ED
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＤＣＭ出荷確定データ >>--*
 FD  KPSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     KPSYUKF   OF        XFDLIB
              JOINING   KMK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*#2019/03/19 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL1           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/19 NAV ED
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
 01  KPSYUKF-ST        PIC  X(02).
*#2019/03/19 NAV ST
 01  DCMHSBL1-ST       PIC  X(02).
*#2019/03/19 NAV ED
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
*伝票番号変換
 01  WK-DENPYO          PIC  X(07).
 01  FILLER             REDEFINES      WK-DENPYO.
     03  WK-DENPYO-R    PIC  9(07).
*
 01  WK-SURYO           PIC  9(05)V9   VALUE  ZERO.
 01  WK-SYUKA           PIC  9(05)V9   VALUE  ZERO.
 01  CNT-AFTER          PIC  9(02)     VALUE  ZERO.
 01  CHK-FLG            PIC  X(03)     VALUE  SPACE.
*#2019/03/19 NAV ST
 01  DCMHSBL1-INV-FLG   PIC  X(03)     VALUE  SPACE.
*#2019/03/19 NAV ED
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
     03  WK-KMK-K01     PIC  9(08).
     03  WK-KMK-K02     PIC  9(04).
     03  WK-KMK-K03     PIC  9(08).
     03  WK-KMK-K04     PIC  X(02).
     03  WK-KMK-F09     PIC  9(02).
     03  WK-KMK-F05     PIC  9(08).
     03  WK-KMK-K08     PIC  9(08).
     03  WK-KMK-F21     PIC  9(04).
     03  WK-KMK-F264    PIC  X(02).
     03  WK-KMK-M091    PIC  X(01).
     03  WK-KMK-F03     PIC  X(07).
     03  WK-KMK-F03-1   PIC  X(07).
     03  WK-KMK-M03     PIC  9(03).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(105)    VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"　頁　：".
         05  HD00-PAGE  PIC  Z,ZZ9.
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-TOKNM PIC  N(09).
         05  FILLER     PIC  X(34)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-2-5.
         05  FILLER     PIC  N(05)     VALUE
             NC"欠品連絡書".
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(37)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発行日：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-SYSYY PIC  9(04).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-SYSMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-SYSDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
 01  HD02.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(46)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-1     PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-2     PIC  N(10).
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"取引先名称".
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品先".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品先名".
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"形状区分".
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注日：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-HACYY PIC  9(04).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-HACMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-HACDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD04-TOKCD PIC  999999.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  HD04-JISYA PIC  X(20).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD04-TENCD PIC  9999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD04-TENNM PIC  X(20).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD04-KEIJY PIC  N(03).
         05  FILLER     PIC  X(34)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品日：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD04-SYUYY PIC  9(04).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD04-SYUMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD04-SYUDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注番号".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"行_".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"商品名".
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"規格".
         05  FILLER     PIC  X(21)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注数量".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品数量".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"欠品数量".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"欠品理由".
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
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  999999999.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  MS01-GYO   PIC  Z9.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  MS01-BUMON PIC  999.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  MS01-JANCD PIC  X(13).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  MS01-SYONM1 PIC X(20).
         05  MS01-SYONM2 PIC X(20).
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  MS01-HACSU PIC  -,---,--9.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  MS01-NOUSU PIC  -,---,--9.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  MS01-KEPSU PIC  -,---,--9.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  MS01-KEPKBN PIC X(02).
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カーマ出荷確定データ >>--*
 KPSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KPSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8866L KPSYUKF ERROR " KPSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    KPSYUKF  DCMHSBL1.
     STOP     RUN.
*#2019/03/19 NAV ST
*----<< 発注種別変換マスタ >>--*
 DCMHSBF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMHSBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8869L DCMHSBL1 ERROR " DCMHSBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     CLOSE    KPSYUKF  DCMHSBL1.
     STOP     RUN.
*#2019/03/19 NAV ED
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
     DISPLAY  "*** SSY8866L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     KPSYUKF.
     OPEN     OUTPUT    PRTF.
*#2019/03/19 NAV ST
     OPEN     INPUT     DCMHSBL1.
*#2019/03/19 NAV ED
*
     INITIALIZE  BREAK-KEY.
*欠品明細データ読込
     PERFORM  900-KMK-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     KMK-K01    TO   WK-KMK-K01
              MOVE     KMK-K02    TO   WK-KMK-K02
              MOVE     KMK-K03    TO   WK-KMK-K03
              MOVE     KMK-K04    TO   WK-KMK-K04
              MOVE     KMK-F09    TO   WK-KMK-F09
              MOVE     KMK-F05    TO   WK-KMK-F05
              MOVE     KMK-K08    TO   WK-KMK-K08
              MOVE     KMK-F21    TO   WK-KMK-F21
              MOVE     KMK-F264   TO   WK-KMK-F264
              MOVE     KMK-M091   TO   WK-KMK-M091
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT  PAGE-CNT2
              MOVE     SPACE      TO   CHK-FLG
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    発注伝票区分／発注日／出荷日／店ＣＤ／館ＣＤブレイク時
*    改ページ
     IF       KMK-F09   NOT =  WK-KMK-F09
     OR       KMK-F05   NOT =  WK-KMK-F05
     OR       KMK-K08   NOT =  WK-KMK-K08
     OR       KMK-F21   NOT =  WK-KMK-F21
     OR       KMK-F264  NOT =  WK-KMK-F264
     OR       KMK-M091  NOT =  WK-KMK-M091
              PERFORM   HEAD-WT-SEC
              MOVE      KMK-F09   TO  WK-KMK-F09
              MOVE      KMK-F05   TO  WK-KMK-F05
              MOVE      KMK-K08   TO  WK-KMK-K08
              MOVE      KMK-F21   TO  WK-KMK-F21
              MOVE      KMK-F264  TO  WK-KMK-F264
              MOVE      KMK-F03(1:7)   TO  WK-KMK-F03
              MOVE      KMK-M091  TO  WK-KMK-M091
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
     CLOSE    KPSYUKF  PRTF.
*#2019/03/19 NAV ST
     CLOSE    DCMHSBL1.
*#2019/03/19 NAV ED
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8866L END *** "
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
     ADD      1         TO        PAGE-CNT.
*ページ数セット
     MOVE     PAGE-CNT      TO   HD00-PAGE.
*システム日付
     MOVE     SYS-YYW       TO   HD01-SYSYY.
     MOVE     SYS-MMW       TO   HD01-SYSMM.
     MOVE     SYS-DDW       TO   HD01-SYSDD.
*取引先名称
     EVALUATE  KMK-K03
         WHEN  880  WHEN 882  WHEN  883  WHEN  1427  WHEN  14272
         WHEN  14273
         MOVE  NC"ホーマック株式会社"  TO  HD01-TOKNM
*#2017/03/21 NV ST
*********WHEN  13938  WHEN  17137
         WHEN  13938  WHEN  17137  WHEN  139381  WHEN  171371
*#2017/03/21 NV ED
         MOVE  NC"株式会社　カーマ　"  TO  HD01-TOKNM
         WHEN  100403  WHEN  100404  WHEN  100427  WHEN  100428
         WHEN  100441  WHEN  100442
         MOVE  NC"ダイキ　株式会社　"  TO  HD01-TOKNM
*#2018/02/19 NV ST
         WHEN  1731   WHEN  1732   WHEN  7601    WHEN  7602
         MOVE  NC"株式会社ケーヨー　"  TO  HD01-TOKNM
*#2018/02/19 NV ED
         WHEN  OTHER
         MOVE  ALL NC"＊"              TO  HD01-TOKNM
     END-EVALUATE.
*    発注種別区分
     MOVE     KMK-F09        TO   HD02-1.
     EVALUATE KMK-F09
         WHEN 0
              MOVE NC"　定　期　　　　　　" TO HD02-2
         WHEN 1
              MOVE NC"　特　売　　　　　　" TO HD02-2
         WHEN 2
              MOVE NC"　新店改装　　　　　" TO HD02-2
         WHEN 3
              MOVE NC"　投　入　　　　　　" TO HD02-2
         WHEN 4
              MOVE NC"　ダイレクト　　　　" TO HD02-2
         WHEN 5
              MOVE NC"　客注ダイレクト　　" TO HD02-2
         WHEN 6
              MOVE NC"　新規初回　　　　　" TO HD02-2
         WHEN 51
              MOVE NC"　本部在庫補充　　　" TO HD02-2
         WHEN 52
              MOVE NC"商管在庫補充：預かり" TO HD02-2
         WHEN 71
              MOVE NC"　備品（用度品）　　" TO HD02-2
*********2017/04/10 NAV ST
         WHEN 81
              MOVE NC"　プロモーション　　" TO HD02-2
         WHEN 82
              MOVE NC"　改　廃　　　　　　" TO HD02-2
         WHEN 83
              MOVE NC"　Ｂ　Ｙ　改　廃　　" TO HD02-2
*********2017/04/10 NAV ED
     END-EVALUATE.
*#2019/02/21 NAV ED
*#2019/03/19 NAV ST 発注種別変換マスタより取得
     MOVE     KMK-K03                      TO   HSB-F01.
     MOVE     KMK-F09                      TO   HSB-F02.
     PERFORM  DCMHSBL1-READ-SEC.
     IF  DCMHSBL1-INV-FLG  =  SPACE
              MOVE   HSB-F09               TO   HD02-2
     ELSE
              MOVE   NC"発注種別未登録"    TO   HD02-2
     END-IF.
*    ベンダーＣＤ
     MOVE     KMK-F304       TO   HD04-TOKCD.
*    ベンダー名称
     MOVE     KMK-F29        TO   HD04-JISYA.
*    店ＣＤ
     MOVE     KMK-F21        TO   HD04-TENCD.
*    店舗名称
     MOVE     KMK-F22        TO   HD04-TENNM.
*    形状区分
     EVALUATE KMK-M091
         WHEN "1"
              MOVE NC"小物　"       TO  HD04-KEIJY
         WHEN "2"
              MOVE NC"異形　"       TO  HD04-KEIJY
         WHEN "3"
              MOVE NC"ケース"       TO  HD04-KEIJY
         WHEN OTHER
              MOVE NC"＊＊＊"       TO  HD04-KEIJY
     END-EVALUATE.
*    発注日
     MOVE     KMK-F05(1:4)   TO   HD03-HACYY.
     MOVE     KMK-F05(5:2)   TO   HD03-HACMM.
     MOVE     KMK-F05(7:2)   TO   HD03-HACDD.
*    出荷日
     MOVE     KMK-K08(1:4)   TO   HD04-SYUYY.
     MOVE     KMK-K08(5:2)   TO   HD04-SYUMM.
     MOVE     KMK-K08(7:2)   TO   HD04-SYUDD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD00  AFTER  2.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  2.
     WRITE    PRT-REC  FROM  HD04  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  2.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  1.
*行カウント
     MOVE     11             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  51
            PERFORM   HEAD-WT-SEC
     END-IF.
*    発注伝票番号
     EVALUATE  KMK-K03
         WHEN  880  WHEN 882  WHEN  883  WHEN  1427  WHEN  14272
         WHEN  14273
         MOVE  KMK-F03            TO   WK-DENPYO
         MOVE  WK-DENPYO-R        TO   MS01-DENNO
         WHEN  OTHER
         MOVE  KMK-F03            TO   MS01-DENNO
     END-EVALUATE.
*    行
     MOVE   KMK-M03               TO   MS01-GYO.
*    部門ＣＤ
     MOVE   KMK-F14               TO   MS01-BUMON.
*    ＪＡＮＣＤ
     MOVE   KMK-M02               TO   MS01-JANCD.
*    品名
     MOVE   KMK-M07(1:20)         TO   MS01-SYONM1.
*    規格
     MOVE   KMK-M07(21:15)        TO   MS01-SYONM2.
*    発注数
     IF     KMK-M11  >  ZERO
            COMPUTE  MS01-HACSU  =  KMK-M11  /  100
     ELSE
            MOVE     ZERO         TO   MS01-HACSU
     END-IF.
*    出荷数
     MOVE   KMK-A36               TO   MS01-NOUSU.
*    欠品数
*****MOVE   KMK-M10               TO   MS01-KEPSU.
     COMPUTE  MS01-KEPSU  =  ( KMK-M11 / 100 ) - KMK-A36.
*    欠品理由
     MOVE   KMK-M20               TO   MS01-KEPKBN.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  2.
     ADD       1           TO   LINE-CNT.
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    カーマ出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-KMK-READ           SECTION.
     MOVE     "900-KMK-READ"      TO   S-NAME.
*
     READ     KPSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-KMK-READ-EXIT
     END-READ.
*
     ADD      1         TO        READ-CNT.
*
 900-KMK-READ-EXIT.
     EXIT.
*#2019/03/19 NAV ST
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL1-READ-SEC         SECTION.
*
     READ     DCMHSBL1
         INVALID
           MOVE  "INV"     TO        DCMHSBL1-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL1-INV-FLG
     END-READ.
*
 DCMHSBL1-READ-EXIT.
     EXIT.
*#2019/03/19 NAV ED
*-----------------<< PROGRAM END >>----------------------------*
