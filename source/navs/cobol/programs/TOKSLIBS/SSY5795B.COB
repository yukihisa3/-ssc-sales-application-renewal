****************************************************************
*    顧客名　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　：　グッデイＥＤＩ
*    業務名　　　　　　：　出荷業務
*    モジュール名　　　：　出荷送信ファイル初期化判定
*    作成日／更新日　　：　2014/06/16
*    作成者／更新者　　：　NAV
*    処理概要　　　　　：　出荷送信Ｆの初期化チェックを行なう。
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY5795B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/16.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<送信ﾁｪｯｸﾌｧｲﾙ>>----*
     SELECT   PCCHK5    ASSIGN         DA-01-S-PCCHK5
                        ORGANIZATION   SEQUENTIAL
                        STATUS         CHK-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<送信ﾁｪｯｸﾌｧｲﾙ>>----*
 FD  PCCHK5
                        BLOCK CONTAINS 1  RECORDS.
 01  CHK-REC.
     03   CHK-F01       PIC  X(02).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-STATUS.
     03  CHK-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SSY5795B".
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
*リンケージ
 LINKAGE                SECTION.
 01  PARA-CHK                PIC 9(01).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-CHK.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 PCCHK5-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCCHK5.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY5795B  PCCHK5 ERROR " CHK-ST " "
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
     PERFORM  200-MAIN-RTN.
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
     DISPLAY  "*** SSY5795B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     PCCHK5.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*送信ﾁｪｯｸﾌｧｲﾙ初期化
     READ     PCCHK5   AT  END
              MOVE   ZERO   TO   PARA-CHK
              GO            TO   200-MAIN-RTN-EXIT
     END-READ.
*ﾚｺｰﾄﾞ存在するとき
     IF       CHK-F01  =  "OK"
              MOVE   1      TO   PARA-CHK
     ELSE
              MOVE   ZERO   TO   PARA-CHK
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルＣＬＯＳＥ
     CLOSE    PCCHK5.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY5795B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
