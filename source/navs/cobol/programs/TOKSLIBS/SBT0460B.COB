****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携（カインズ）        *
*    モジュール名　　　　：　出荷梱包送信結果チェック          *
*    作成日／更新日　　　：　2014/08/08                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷梱包ＭＳＧの送信結果チェック　*
*                        ：　を行なう。                        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0460B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/08/08.
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
     SELECT   PCCHKC   ASSIGN         DA-01-S-PCCHKC
                        ORGANIZATION   SEQUENTIAL
                        STATUS         CHK-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<送信ﾁｪｯｸﾌｧｲﾙ>>----*
 FD  PCCHKC
                        BLOCK CONTAINS 1  RECORDS.
 01  CHK-REC.
     03   CHK-F01       PIC  X(02).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-STATUS.
     03  CHK-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SBT0460B".
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
 PCCHKC-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCCHKC.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBT0460B PCCHKC  ERROR " CHK-ST " "
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
     DISPLAY  "*** SBT0460B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     PCCHKC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*送信ﾁｪｯｸﾌｧｲﾙ初期化
     READ     PCCHKC   AT  END
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
     CLOSE    PCCHKC.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "KEKKA=" PARA-CHK UPON CONS.
     DISPLAY  "*** SBT0460B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
