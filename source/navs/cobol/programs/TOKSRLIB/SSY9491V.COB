****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシカメラ　　　　　　　　　　*
*    モジュール名　　　　：　ＥＸＣＥＬ納期数量一括変更結果出力*
*    作成日／作成者　　　：　2023/04/18 NAV TAKAHASHI          *
*    処理概要　　　　　　：　ＥＸＣＥＬ納期数量一括変更結果の出*
*                          　力（ＣＳＶ）を出力する。　　　　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2023/04/18 TAKAHASHI新規作成  流用：SSY9491V.TOKSRLIB
* 2023/06138 TAKAHASHI仕様変更  最大月数２→４月へ変更　
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9491V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/04/12.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<<納期数量一括変更取込チェック>>--*
     SELECT   YODHECKF  ASSIGN    TO   DA-01-S-YODHECKF
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   HEC-ST.
*----<<納期数量一括変更チェックＣＳＶ>>---*
     SELECT   YODHECCV  ASSIGN    TO   DA-01-S-YODHECCV
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   CCV-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<納期数量一括変更取込チェック>>--*
 FD  YODHECKF             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  HECWK-REC.
     03  FILLER                   PIC   X(600).
*----<<納期数量一括変更ＣＳＶ>>-*
 FD  YODHECCV             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  HECCV-REC.
     03  FILLER                   PIC   X(1000).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HEC-ST            PIC  X(02)        VALUE  SPACE.
 01  CCV-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  RD-CNT            PIC  9(07)        VALUE  ZERO.
 01  CCV-CNT           PIC  9(07)        VALUE  ZERO.
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
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
 01  GETUDO                  PIC  9(02).
 01  GETUDO2                 PIC S9(02).
 01  GETUDO2-YMD.
     03  GETUDO2-YY          PIC  9(04).
     03  GETUDO2-MM          PIC  9(02).
     03  GETUDO2-DD          PIC  9(02).
 01  GETUDO2-YMDR      REDEFINES  GETUDO2-YMD.
     03  GETUDO2-YYMMR       PIC  9(06).
     03  FILLER              PIC  X(02).
 01  ACOS-DATE               PIC  9(08).
 01  FILLER                  REDEFINES      ACOS-DATE.
     03  ACOS-YYMM           PIC  9(06).
     03  ACOS-DD             PIC  9(02).
 01  START-YYMM              PIC  9(06).
 01  FILLER                  REDEFINES      START-YYMM.
     03  START-YY            PIC  9(04).
     03  START-MM            PIC  9(02).
 01  END-YYMM                PIC  9(06).
 01  FILLER                  REDEFINES      END-YYMM.
     03  END-YY              PIC  9(04).
     03  END-MM              PIC  9(02).
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  SAV-NOU-DATE2           PIC  9(08)    VALUE ZERO.
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-HNOU-DATE2          PIC  9(08)    VALUE ZERO.
 01  SAV-HNOU-DATE.
     03  SAV-HNOU-YYMM       PIC  9(06).
     03  SAV-HNOU-DD         PIC  9(02).
 01  SAV-HTENCD              PIC  9(05)    VALUE ZERO.
 01  SAV-CYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-SYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-ZENGETU             PIC  9(08).
 01  SAV-ACOS-YYMM.
     03  SAV-ACOS-YY         PIC  9(04).
     03  SAV-ACOS-MM         PIC  9(02).
 01  SAV-ACOS-MMS            PIC  9(02).
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
***検収日開始日
 01  KAISI-YMD               PIC  9(08).
 01  FILLER                  REDEFINES   KAISI-YMD.
     03  KAISI-YY            PIC  9(04).
     03  KAISI-MM            PIC  9(02).
     03  KAISI-DD            PIC  9(02).
***検収日終了日
 01  SYURYO-YMD              PIC  9(08).
 01  FILLER                  REDEFINES   SYURYO-YMD.
     03  SYURYO-YY           PIC  9(04).
     03  SYURYO-MM           PIC  9(02).
     03  SYURYO-DD           PIC  9(02).
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  PRT-HEAD01.
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(17)
         VALUE  NC"＜納期数量一括変更チェックデータ＞".
     03  FILLER  PIC X(01) VALUE X"29".
 01  PRT-HEAD02.
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"取込日付：".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  HD2-TDT PIC X(10).
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"取込時刻：".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  HD2-TTM PIC X(08).
 01  PRT-HEAD03.
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(08) VALUE NC"メッセージタイプ".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"購買発注番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"発注伝票日付".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"仕入先コード".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"発注先コード".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"出荷先コード".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"明細行番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"品日番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(03) VALUE NC"商品名".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(10) VALUE NC"明細フリーテキスト１".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"明細単価".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(07) VALUE NC"お客様伝票番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"配送場所".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"発注数量".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"納入期日".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"バッチ日付".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"バッチ時刻".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"バッチ取引先".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"基幹伝票番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(05) VALUE NC"基幹行番号".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(08) VALUE NC"取込担当部門ＣＤ".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(07) VALUE NC"取込担当者ＣＤ".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（売上未存在）　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（基本未存在）　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（納品日論理）　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（当月以前）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（４ヶ月後）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（数量変更）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（確定区分）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備８）　　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備９）　　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１０）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１１）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１２）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１３）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１４）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(13) VALUE NC"ＥＲＲ（予備１５）　　　　".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(06) VALUE NC"出荷確区分".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"変更納期".
     03  FILLER  PIC X(01) VALUE X"29".
     03  FILLER  PIC X(01) VALUE ",".
     03  FILLER  PIC X(01) VALUE X"28".
     03  FILLER  PIC N(04) VALUE NC"変更数量".
     03  FILLER  PIC X(01) VALUE X"29".
*
 01  SEC-AREA.
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
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                    SECTION.
 01  LINK-IN-TDATE          PIC 9(08).
 01  LINK-IN-TTIME          PIC 9(06).
 01  LINK-IN-BUMONCD        PIC 9(08).
 01  LINK-IN-TANCD          PIC X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-TDATE
                                  LINK-IN-TTIME
                                  LINK-IN-BUMONCD
                                  LINK-IN-TANCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<<納期数量一括変更取込チェック>>--*
 YODHECKF-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODHECKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9491V YODHECKF ERROR " HEC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<<納期数量一括変更チェックＣＳＶ>>---*
 YODHECCV-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODHECCV.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9491V YODHECCV ERROR " CCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
 100-INIT-01.
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
*
     ACCEPT   SYS-TIME       FROM TIME.
*
 100-INIT-02.
     OPEN     INPUT     YODHECKF.
     OPEN     OUTPUT    YODHECCV.
*
 100-INIT-03.
*　　納期数量一括変更取込チェックＲＥＡＤ
     PERFORM YODHECKF-READ-SEC.
     IF      END-FLG    =     "END"
             DISPLAY NC"納期数量一括変更取込Ｆ無"  UPON CONS
             GO      TO     100-INIT-RTN-EXIT
     ELSE
             MOVE    SPACE                TO   HECCV-REC
             MOVE    PRT-HEAD01           TO   HECCV-REC
             WRITE                             HECCV-REC
             ADD     1                    TO   CCV-CNT
             MOVE    SPACE                TO   HECCV-REC
             MOVE    LINK-IN-TDATE(1:4)   TO   HD2-TDT(1:4)
             MOVE    "/"                  TO   HD2-TDT(5:1)
             MOVE    LINK-IN-TDATE(5:2)   TO   HD2-TDT(6:2)
             MOVE    "/"                  TO   HD2-TDT(8:1)
             MOVE    LINK-IN-TDATE(7:2)   TO   HD2-TDT(9:2)
             MOVE    LINK-IN-TTIME(1:2)   TO   HD2-TTM(1:2)
             MOVE    ":"                  TO   HD2-TTM(3:1)
             MOVE    LINK-IN-TTIME(3:2)   TO   HD2-TTM(4:2)
             MOVE    ":"                  TO   HD2-TTM(6:1)
             MOVE    LINK-IN-TTIME(5:2)   TO   HD2-TTM(7:2)
             MOVE    PRT-HEAD02           TO   HECCV-REC
             WRITE                             HECCV-REC
             ADD     1                    TO   CCV-CNT
             MOVE    SPACE                TO   HECCV-REC
             MOVE    PRT-HEAD03           TO   HECCV-REC
             WRITE                             HECCV-REC
             ADD     1                    TO   CCV-CNT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理　　　　　　　　　                *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*明細セット
     MOVE    SPACE                TO   HECCV-REC.
     MOVE    HECWK-REC            TO   HECCV-REC.
     WRITE                             HECCV-REC.
     ADD     1                    TO   CCV-CNT.
*売上伝票ファイルＲＥＡＤ
 200-MAIN-06.
     PERFORM  YODHECKF-READ-SEC.
*
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    YODHECKF  YODHECCV.
*
     DISPLAY  NC"納期数量一括ＣＨＫ＝" RD-CNT  NC"件"
                                                     UPON CONS.
     DISPLAY  NC"納期数量一括ＣＳＶ＝" CCV-CNT NC"件"
                                                     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期数量一括変更取込チェック
*--------------------------------------------------------------*
 YODHECKF-READ-SEC       SECTION.
     MOVE  "YODHECKF-READ-SEC"     TO  S-NAME.
*
     READ   YODHECKF
               AT  END
                        MOVE "END" TO  END-FLG
                        GO         TO  YODHECKF-READ-EXIT
     END-READ.
*
     ADD       1                   TO  RD-CNT.
     IF  RD-CNT(5:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT      UPON CONS
     END-IF.
*
 YODHECKF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
