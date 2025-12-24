****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ホームワイド支払照合　　　　　　　*
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　00/02/14                          *
*    作成者／更新者　　　：　HAGIWARA                          *
*    処理概要　　　　　　：　支払情報ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0200L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          00/02/14.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKF  ASSIGN         DA-01-S-SITGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         GKF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKF           LABEL RECORD   IS   STANDARD.
 01  HED-REC.
     03  HED-F01        PIC  X(01).
     03  HED-F02        PIC  X(04).
     03  HED-F03        PIC  9(06).
     03  HED-F04        PIC  X(24).
     03  HED-F05        PIC  9(06).
     03  HED-F06        PIC  9(06).
     03  HED-F07        PIC  X(17).
*
 01  BDY-REC.
     03  BDY-F01        PIC  X(01).
     03  BDY-F02        PIC  9(04).
     03  BDY-F03        PIC  9(06).
     03  BDY-F04        PIC  9(05).
     03  BDY-F05        PIC  9(06).
     03  BDY-F06        PIC  9(05).
     03  BDY-F07        PIC  9(02).
     03  BDY-F08        PIC  9(11).
     03  BDY-F09        PIC  9(10).
     03  BDY-F10        PIC  9(02).
     03  BDY-F11        PIC  9(02).
     03  BDY-F12        PIC  9(10).
*
 01  TAL-REC.
     03  TAL-F01        PIC  X(01).
     03  TAL-F02        PIC  X(25).
     03  TAL-F03        PIC  9(11).
     03  TAL-F04        PIC  9(09).
     03  TAL-F05        PIC  9(09).
     03  TAL-F06        PIC  9(09).

*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
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
 01  COUNT-AREA.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
 01  FLG-AREA.
     03  END-FLG        PIC  9(01)     VALUE  ZERO.
     03  RD-FLG         PIC  9(01)     VALUE  ZERO.
 01  WORK-AREA.
     03  WK-TENCD       PIC  9(05)     VALUE  ZERO.
     03  WK-TORICD      PIC  9(06)     VALUE  ZERO.
     03  WK-KIKAN1      PIC  9(02)     VALUE  ZERO.
     03  WK-KIKAN2      PIC  9(02)     VALUE  ZERO.
     03  WK-KIKAN3      PIC  9(02)     VALUE  ZERO.
     03  WK-KIKAN4      PIC  9(02)     VALUE  ZERO.
     03  WK-KIKAN5      PIC  9(02)     VALUE  ZERO.
     03  WK-KIKAN6      PIC  9(02)     VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI0200L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  GKF-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER.
         05  FILLER     PIC  X(25)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-15.
         05  FILLER     PIC  N(09)     VALUE
                        NC"【　支払明細書　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE
                        NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD1-011    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD1-012    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD1-013    PIC  9(02).
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  HD1-02     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE
                        NC"頁".
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE
                        NC"取引先コード".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD2-01     PIC  9(06).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD2-02     PIC  N(10).
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE
                        NC"対象期間".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD2-031    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD2-032    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD2-033    PIC  9(02).
         05  FILLER     PIC  N(01)     VALUE
                        NC"～".
         05  HD2-041    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD2-042    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD2-043    PIC  9(02).
*
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE
                        NC"店舗".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE
                        NC"納品日".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE
                        NC"伝票区分".
         05  FILLER     PIC  X(19)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE
                        NC"伝票番号".
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE
                        NC"金額".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE
                        NC"支払区分".
 01  SEN1.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
         05  FILLER     PIC  N(01)     VALUE
             NC"_".

 01  SEN2.
     03  FILLER.
         05  FILLER     PIC  X(40)     VALUE
             "----------------------------------------".
         05  FILLER     PIC  X(40)     VALUE
             "----------------------------------------".
         05  FILLER     PIC  X(02)     VALUE
             "--".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-15.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME1-01     PIC  9(05).
         05  FILLER     PIC  X(03).
         05  ME1-02     PIC  N(10).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE PITCH-15.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  ME2-011    PIC  9(02).
         05  ME2-P1     PIC  X(01)     VALUE  "/".
         05  ME2-012    PIC  9(02).
         05  ME2-P2     PIC  X(01)     VALUE  "/".
         05  ME2-013    PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-02     PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-03     PIC  N(15).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-04     PIC  9(11).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-05     PIC  ----,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-06     PIC  9(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME2-07     PIC  N(09).
*
 01  TAIL01.
     03  FILLER         CHARACTER TYPE PITCH-15.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"金額１：".
         05  TL1-01     PIC  ---,---,--9.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"金額２：".
         05  TL1-02     PIC  ---,---,--9.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"金額３：".
         05  TL1-03     PIC  ---,---,--9.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(07)  VALUE  NC"支払金額合計：".
         05  TL1-04     PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITGKF  ERROR " GKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTOKMS   ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
 END DECLARATIVES.
*--------------------------------------------------------------*
*                  ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                      0.0     *
*--------------------------------------------------------------*
 PROCESS-START          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL     END-FLG = 9.
     PERFORM  END-SEC.
*
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 PROCESS-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             初期処理                                1.0      *
*--------------------------------------------------------------*
 INIT-SEC               SECTION.
*ファイルのオープン
     OPEN     INPUT     SITGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     99             TO   LINE-CNT.
*支払合計ファイル初期読込み
     PERFORM  GKF-READ-SEC.
*
 INIT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             メイン処理                              2.0      *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.

     EVALUATE      HED-F01
         WHEN "H"
              PERFORM   HEAD-WRITE1-SEC
         WHEN "B"
***           ブレイク判定
                   IF   LINE-CNT  >=   62
                        MOVE      SPACE    TO    PRT-REC
                        WRITE     PRT-REC  AFTER PAGE
                        PERFORM   HEAD-WRITE2-SEC
                        MOVE      ZERO     TO    RD-FLG
                   END-IF
*
              PERFORM   BODY-WRITE-SEC
***         明細レコード１件目時処理
              IF       RD-FLG   =     ZERO
*                  MOVE      BDY-F06   TO   WK-TENCD
                   MOVE      1         TO   RD-FLG
              END-IF
*
         WHEN "T"
              PERFORM   TAIL-WRITE-SEC
     END-EVALUATE.

     PERFORM  GKF-READ-SEC.

*
 MAIN-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             明細行出力処理                          2.1      *
*--------------------------------------------------------------*
 BODY-WRITE-SEC         SECTION.
*
     MOVE     SPACE          TO   MEIS01  MEIS02.
*明細１行目
     IF       LINE-CNT       =    8
         OR   WK-TENCD       NOT  =    BDY-F06
*        店舗コード
         MOVE      BDY-F06        TO   ME1-01
*        店舗名称取得
         MOVE      BDY-F03        TO   TEN-F52
         MOVE      BDY-F06        TO   TEN-F011
         PERFORM   TEN-READ-SEC
         IF        TEN-F03   =  SPACE
                   MOVE      ALL NC"＊"     TO   ME1-02
         ELSE
                   MOVE      TEN-F03        TO   ME1-02
         END-IF
***    明細一行目以外，店ブレイク時点線
         IF        RD-FLG    NOT = ZERO
              WRITE     PRT-REC   FROM   SEN2    AFTER     1
              ADD       1         TO   LINE-CNT
         END-IF
***
         WRITE     PRT-REC   FROM MEIS01    AFTER     1
         ADD       1         TO   LINE-CNT

         MOVE      BDY-F06        TO   WK-TENCD
     END-IF.
*明細２行目
*    納品日
     MOVE     BDY-F05(1:2)   TO   ME2-011.
     MOVE     "/"            TO   ME2-P1.
     MOVE     BDY-F05(3:2)   TO   ME2-012.
     MOVE     "/"            TO   ME2-P2.
     MOVE     BDY-F05(5:2)   TO   ME2-013.
*    伝票区分
     MOVE     BDY-F07        TO   ME2-02.
     EVALUATE BDY-F07
         WHEN   "11"
         MOVE   NC"仕入"                    TO   ME2-03
         WHEN   "31"
         MOVE   NC"返品"                    TO   ME2-03
         WHEN   "33"
         MOVE   NC"値引き"                  TO   ME2-03
         WHEN   "41"
         MOVE   NC"ＥＯＳ処理料控除"        TO   ME2-03
         WHEN   "42"
         MOVE   NC"請求照合処理料控除"      TO   ME2-03
         WHEN   "43"
         MOVE   NC"センターフィー控除"      TO   ME2-03
         WHEN   "44"
         MOVE   NC"発注未遅納・誤納控除"    TO   ME2-03
         WHEN   "45"
         MOVE   NC"相殺"                    TO   ME2-03
         WHEN   "46"
         MOVE   NC"リベート"                TO   ME2-03
         WHEN   "59"
         MOVE   NC"その他控除"              TO   ME2-03
         WHEN   "61"
         MOVE   NC"仕入・返品・値引に関する消費税"   TO   ME2-03
         WHEN   "62"
         MOVE   NC"当社控除に対する消費税"  TO   ME2-03
         WHEN   "99"
         MOVE   NC"前月支払分調整"          TO   ME2-03
         WHEN   OTHER
         MOVE   NC"　　　　"                TO   ME2-03
     END-EVALUATE.
*伝票_
     MOVE     BDY-F08        TO   ME2-04.
*支払金額
     MOVE     BDY-F09        TO   ME2-05.
*支払区分
     MOVE     BDY-F11        TO   ME2-06.
     EVALUATE BDY-F11
         WHEN  0
         MOVE NC"請求額通り支払"       TO ME2-07
         WHEN  1
         MOVE NC"支払差異あり"         TO ME2-07
         WHEN  2
         MOVE NC"未照合にて支払保留"   TO ME2-07
     END-EVALUATE.
*
     WRITE     PRT-REC   FROM MEIS02    AFTER     1.
     ADD       1         TO   LINE-CNT.
*
 BODY-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ＨＥＡＤ出力処理１                      2.2      *
*--------------------------------------------------------------*
 HEAD-WRITE1-SEC         SECTION.
*ページカウンター
     ADD      1              TO   PAGE-CNT.
*システム日付
     MOVE     SYS-YY         TO   HD1-011.
     MOVE     SYS-MM         TO   HD1-012.
     MOVE     SYS-DD         TO   HD1-013.
*頁
     MOVE     PAGE-CNT       TO   HD1-02.
*取引先コード
     MOVE     HED-F03        TO   HD2-01  WK-TORICD.
*取引先名取得
     MOVE     HED-F03        TO   TOK-F01.
     PERFORM  TOK-READ-SEC.
     MOVE     TOK-F03        TO   HD2-02.
*対象期間(FROM)
     MOVE     HED-F05(1:2)   TO   HD2-031 WK-KIKAN1.
     MOVE     HED-F05(3:2)   TO   HD2-032 WK-KIKAN2.
     MOVE     HED-F05(5:2)   TO   HD2-033 WK-KIKAN3.
*対象期間(TO)
     MOVE     HED-F06(1:2)   TO   HD2-041 WK-KIKAN4.
     MOVE     HED-F06(3:2)   TO   HD2-042 WK-KIKAN5.
     MOVE     HED-F06(5:2)   TO   HD2-043 WK-KIKAN6.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      SEN1      AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      SEN1      AFTER     1.
     MOVE     8              TO   LINE-CNT.
*
 HEAD-WRITE1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ＨＥＡＤ出力処理２                      2.3      *
*--------------------------------------------------------------*
 HEAD-WRITE2-SEC        SECTION.
*ページカウンター
     ADD      1              TO   PAGE-CNT.
*システム日付
     MOVE     SYS-YY         TO   HD1-011.
     MOVE     SYS-MM         TO   HD1-012.
     MOVE     SYS-DD         TO   HD1-013.
*頁
     MOVE     PAGE-CNT       TO   HD1-02.
*取引先コード
     MOVE     WK-TORICD      TO   HD2-01.
*取引先名
     MOVE     WK-TORICD      TO   TOK-F01.
     PERFORM  TOK-READ-SEC.
     MOVE     TOK-F03        TO   HD2-02.
*対象期間(FROM)
     MOVE     WK-KIKAN1      TO   HD2-031.
     MOVE     WK-KIKAN2      TO   HD2-032.
     MOVE     WK-KIKAN3      TO   HD2-033.
*対象期間(TO)
     MOVE     WK-KIKAN4      TO   HD2-041.
     MOVE     WK-KIKAN5      TO   HD2-042.
     MOVE     WK-KIKAN6      TO   HD2-043.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      SEN1      AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      SEN1      AFTER     1.
     MOVE     8              TO   LINE-CNT.
*
 HEAD-WRITE2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             テイル印字処理                           2.4     *
*--------------------------------------------------------------*
 TAIL-WRITE-SEC         SECTION.

*金額１
     MOVE     TAL-F04        TO   TL1-01.
*金額２
     MOVE     TAL-F05        TO   TL1-02.
*金額３
     MOVE     TAL-F06        TO   TL1-03.
*支払金額合計
     MOVE     TAL-F03        TO   TL1-04.
*テール行印字
     WRITE    PRT-REC   FROM      TAIL01    AFTER     2.
     ADD      2         TO   LINE-CNT.
*
 TAIL-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 TOK-READ-SEC           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 TEN-READ-SEC           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 GKF-READ-SEC           SECTION.
     READ     SITGKF  AT   END
              MOVE      9    TO   END-FLG
              GO   TO   GKF-READ-EXIT
     END-READ.
*
 GKF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             終了処理                               3.0       *
*--------------------------------------------------------------*
 END-SEC                SECTION.
*ファイルのクローズ
     CLOSE    SITGKF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
 END-SEC-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
