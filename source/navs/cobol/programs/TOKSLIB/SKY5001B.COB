****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　電算室殿連携システム              *
*    モジュール名　　　　：　他部門データ抽出                  *
*    作成日／更新日　　　：　2003/07/29                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより受取った部門ＣＤと異な*
*                        ：　るレコードを振替ファイルに出力する*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY5001B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/07/29.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<仕入在庫計上データ>>--------------------*
     SELECT   ZAIKOKEI  ASSIGN         DA-01-S-ZAIKOKEI
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ZAK-ST.
*----<<同一在庫部門間　振替データ>>------------*
     SELECT   ZAIFURIK  ASSIGN         DA-01-S-ZAIFURIK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ZAF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<仕入在庫計上データ>>--------------------*
 FD  ZAIKOKEI
                        BLOCK CONTAINS 1 RECORDS.
 01  ZAK-REC.
     03  ZAK-F01        PIC  X(004).
     03  ZAK-F02        PIC  X(252).
*----<<同一在庫部門間　振替データ>>------------*
 FD  ZAIFURIK
                        BLOCK CONTAINS 1 RECORDS.
 01  ZAF-REC            PIC  X(256).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  ZAK-CNT        PIC  9(07).
     03  ZAF-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  ZAK-ST         PIC  X(02).
     03  ZAF-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY5001B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                SECTION.
 01  LINK-BUMON              PIC X(04).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-BUMON.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ZAIKOKEI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAIKOKEI.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY5001B ZAIKOKEI ERROR " ZAK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 ZAIFURIK-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAIFURIK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY5001B ZAIFURIK ERROR " ZAF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN  UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY5001B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     ZAIKOKEI.
     OPEN     EXTEND    ZAIFURIK.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*初期読込み
     PERFORM       ZAIKOKEI-READ-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*パラメタ部門と比較
     IF      ZAK-F01  NOT =  LINK-BUMON
             MOVE     SPACE     TO      ZAF-REC
             INITIALIZE                 ZAF-REC
             MOVE     ZAK-REC   TO      ZAF-REC
             WRITE    ZAF-REC
             ADD      1         TO      ZAF-CNT
     END-IF.
*
     PERFORM ZAIKOKEI-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    ZAIKOKEI  ZAIFURIK.
*
     DISPLAY  "READ-CNT  = " ZAK-CNT  UPON CONS.
     DISPLAY  "WRITE-CNT = " ZAF-CNT  UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY5001B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    仕入在庫計上データ
*--------------------------------------------------------------*
 ZAIKOKEI-READ-SEC      SECTION.
*
     READ   ZAIKOKEI  AT  END
            MOVE  "END"  TO  END-FLG
            NOT   AT  END
            ADD    1     TO  ZAK-CNT
     END-READ.
*
 ZAIKOKEI-READ-EXIT.
     EXIT.
