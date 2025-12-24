****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルＨＣ  新ＥＤＩシステム  *
*    業務名　　　　　　　：　ロイヤルＨＣ  新ＥＤＩシステム  *
*    モジュール名　　　　：　ロイヤルＨＣ欠品修正リスト発行    *
*    作成日／更新日　　　：　08/11/04                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY7830L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/11/04.
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
*----<< ロイヤルＨＣ欠品修正ファイル>>--*
     SELECT   KEPSYUPF  ASSIGN         DA-01-VI-KEPSYUL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RHC-F03   RHC-F09
                                       RHC-F06   RHC-F10
                                       RHC-F11
                        STATUS         KEPSYUPF-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ロイヤルＨＣ欠品修正ファイル>>--*
 FD  KEPSYUPF           LABEL     RECORD   IS   STANDARD.
     COPY     KEPSYUPF  OF        XFDLIB
              JOINING   RHC       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
 01  KEPSYUPF-ST        PIC  X(02).
 01  HTENMS-ST          PIC  X(02).
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
 01  WK-GENKA           PIC  9(09)V9   VALUE  ZERO.
 01  WK-KEPPIN          PIC  9(05)     VALUE  ZERO.
 01  WK-DENNO           PIC  ZZZZZZZ9.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(11)     VALUE  ZERO.
     03  WK-GK-BAIKA    PIC  9(11)     VALUE  ZERO.
*----<< ﾋﾂﾞｹﾍﾝｼｭｳ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-RHC-F03     PIC  9(08).
     03  WK-RHC-F09     PIC  9(05).
     03  WK-RHC-F06     PIC  9(08).
     03  WK-RHC-F10     PIC  9(09).
     03  WK-RHC-F11     PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD000.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(09)     VALUE
                             NC"＜欠品修正リスト＞".
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  ":".
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
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD01-TENCD PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-TENME PIC  N(15).
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"出荷確定日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"受領日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"自社商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"商品名".
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  FILLER     PIC  N(03)     VALUE  NC"規格名".
         05  FILLER     PIC  X(39)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注数量".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"検収数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  N(03)     VALUE  NC"欠　品".
         05  FILLER     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
 01  SEN                         CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  X(100) VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
         NC"───────".
     03  FILLER                  PIC  X(12)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
         NC"───────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  SEN2.
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(36)  VALUE
         "====================================".
*
 01  MS00.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS00-DENNO PIC  ZZZZZZZ9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS00-HACYY PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-HACMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-HACDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS00-NOHYY PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-NOHMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-NOHDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS00-SHUYY PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-SHUMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-SHUDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS00-JURYY PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-JURMM PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  MS00-JURDD PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ".".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS00-DENKB PIC  X(04).
 01  MS01.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-GYONO PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-SHOCD PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-SHOM1 PIC  X(50).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-HACSU PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-KENSU PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-GENTAN PIC Z,ZZZ,ZZ9.99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-GENKIN PIC ZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-BAITAN PIC Z,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-BAIKIN PIC ZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  MS01-KEPIN PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  MS02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  MS02-SHOM2 PIC  X(50).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"【".
         05  FILLER     PIC  X(03)     VALUE  "RHC".
         05  FILLER     PIC  N(04)     VALUE  NC"自責金額".
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  MS02-JISEK PIC  ZZ,ZZZ,ZZ9.99.
         05  FILLER     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(07)
                             VALUE  NC"修正後納品金額".
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  MS02-GONOH PIC  ZZ,ZZZ,ZZ9.99.
         05  FILLER     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"】".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
 01  MS03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(88)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価合計".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  MS03-GENGOK PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価合計".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  MS03-BAIGOK PIC  ZZZ,ZZZ,ZZ9.
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
*----<< ロイヤルＨＣ欠品修正ファイル >>--*
 KEPSYUPF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEPSYUPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7830L KEPSYUPF ERROR " KEPSYUPF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    KEPSYUPF    HTENMS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7830L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    KEPSYUPF    HTENMS.
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
     DISPLAY  "*** SSY7830L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     KEPSYUPF    HTENMS.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*ロイヤルＨＣ欠品修正ファイルＲＥＡＤ
     MOVE     SPACE               TO   RHC-REC.
     INITIALIZE                        RHC-REC.
     PERFORM  900-RHC-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     RHC-F03    TO   WK-RHC-F03
              MOVE     RHC-F09    TO   WK-RHC-F09
              MOVE     RHC-F06    TO   WK-RHC-F06
              MOVE     RHC-F10    TO   WK-RHC-F10
              MOVE     RHC-F11    TO   WK-RHC-F11
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
*    取引先ＣＤ／店舗ＣＤ
*    ブレイク時改ページ
     IF       RHC-F03   NOT =  WK-RHC-F03
     OR       RHC-F09   NOT =  WK-RHC-F09
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              MOVE      RHC-F03   TO  WK-RHC-F03
              MOVE      RHC-F09   TO  WK-RHC-F09
              MOVE      RHC-F06   TO  WK-RHC-F06
              MOVE      RHC-F10   TO  WK-RHC-F10
              MOVE      RHC-F11   TO  WK-RHC-F11
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       RHC-F10   NOT =  WK-RHC-F10
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
*             PERFORM   MEISAI-HEAD-SEC
              MOVE      RHC-F03   TO  WK-RHC-F03
              MOVE      RHC-F09   TO  WK-RHC-F09
              MOVE      RHC-F06   TO  WK-RHC-F06
              MOVE      RHC-F10   TO  WK-RHC-F10
              MOVE      RHC-F11   TO  WK-RHC-F11
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*
     PERFORM  900-RHC-READ.
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
     CLOSE    KEPSYUPF  HTENMS  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7830L END *** "
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
*    MOVE     PAGE-CNT  TO        HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-H-YYYY      TO   HD00-YYYY.
     MOVE     WK-H-MM        TO   HD00-MM.
     MOVE     WK-H-DD        TO   HD00-DD.
     MOVE     PAGE-CNT       TO   HD00-PAGE.
*    店ＣＤ
     MOVE     RHC-F09        TO   HD01-TENCD.
*    店舗名称
     MOVE     RHC-F03        TO   TEN-F52.
     MOVE     RHC-F09        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F02        TO   HD01-TENME.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD000 AFTER  1.
     WRITE    PRT-REC  FROM  HD00  AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  2.
     WRITE    PRT-REC  FROM  SEN2  AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  SEN2  AFTER  1.
*行カウント
     MOVE      9             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  53
            PERFORM   HEAD-WT-SEC
     END-IF.
*    伝票番号
     MOVE     RHC-F10             TO   MS00-DENNO.
*    発注日
     MOVE     RHC-F05(1:4)        TO   MS00-HACYY.
     MOVE     RHC-F05(5:2)        TO   MS00-HACMM.
     MOVE     RHC-F05(7:2)        TO   MS00-HACDD.
*    納品日
     MOVE     RHC-F06(1:4)        TO   MS00-NOHYY.
     MOVE     RHC-F06(5:2)        TO   MS00-NOHMM.
     MOVE     RHC-F06(7:2)        TO   MS00-NOHDD.
*    出荷確定日
     MOVE     RHC-F08(1:4)        TO   MS00-SHUYY.
     MOVE     RHC-F08(5:2)        TO   MS00-SHUMM.
     MOVE     RHC-F08(7:2)        TO   MS00-SHUDD.
*    受領日
     MOVE     RHC-F07(1:4)        TO   MS00-JURYY.
     MOVE     RHC-F07(5:2)        TO   MS00-JURMM.
     MOVE     RHC-F07(7:2)        TO   MS00-JURDD.
*    伝票区分
     MOVE     RHC-F12             TO   MS00-DENKB.
*    行
     MOVE     RHC-F11             TO   MS01-GYONO.
*    自社商品ＣＤ
     MOVE     RHC-F13             TO   MS01-SHOCD.
*    商品名/規格名
     MOVE     RHC-F14             TO   MS01-SHOM1.
     MOVE     RHC-F15             TO   MS02-SHOM2.
*    発注数量
     MOVE     RHC-F16             TO   MS01-HACSU.
*    検収数量
     MOVE     RHC-F17             TO   MS01-KENSU.
*    原価単価
     MOVE     RHC-F18             TO   MS01-GENTAN.
*    売価単価
     MOVE     RHC-F20             TO   MS01-BAITAN.
*    原価金額
     MOVE     RHC-F19             TO   MS01-GENKIN.
*    売価金額
     MOVE     RHC-F21             TO   MS01-BAIKIN.
*    欠品
     MOVE     RHC-F22             TO   MS01-KEPIN.
*    ＲＨＣ自責金額
     MOVE     RHC-F23             TO   MS02-JISEK.
*    修正後納品金額
     MOVE     RHC-F24             TO   MS02-GONOH.
*
     ADD      RHC-F19             TO   WK-GK-GENKA.
     ADD      RHC-F21             TO   WK-GK-BAIKA.
*
     IF       MS00-DENNO  NOT =   WK-DENNO
              WRITE     PRT-REC   FROM  MS00  AFTER  1
              ADD       1         TO    LINE-CNT
     END-IF.
     WRITE    PRT-REC   FROM  MS01  AFTER  1.
     WRITE    PRT-REC   FROM  MS02  AFTER  1.
     ADD      2           TO   LINE-CNT.
     MOVE     MS00-DENNO           TO   WK-DENNO.
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
     MOVE      SPACE       TO   PRT-REC.
*    WRITE     PRT-REC     AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       2           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    欠品修正ファイル　 START READ                *
*--------------------------------------------------------------*
 900-RHC-READ     SECTION.
     MOVE     "900-RHC-READ"     TO   S-NAME.
     READ     KEPSYUPF  AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-RHC-READ-EXIT
     END-READ.
     ADD      1         TO        READ-CNT.
*
 900-RHC-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F02
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
