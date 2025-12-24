****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　共通処理                          *
*    モジュール名　　　　：　実行場所取得                      *
*    作成日／更新日　　　：　2000/06/05                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　実行を行なっている場所名を条件Ｆ　*
*                        ：　より取得し、パラメタに渡す。      *
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2401B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/20.
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
*----<<条件ファイル>>----*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  JYO-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY2401B".
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
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 JYOERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2401B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
*ファイルのオープン
     OPEN     INPUT     HJYOKEN.
*条件ファイル読込み（”９９”、”ＢＵＭＯＮ”）
     MOVE     99        TO    JYO-F01.
     MOVE     "BUMON"   TO    JYO-F02.
     READ     HJYOKEN  INVALID
              DISPLAY "HJYOKEN INVALID KEY = " JYO-F01 " :"
              JYO-F02  UPON CONS
              STOP  RUN
     END-READ.
*メッセージ
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"   UPON CONS.
     DISPLAY NC"＃　日付：" SYS-YY "." SYS-MM "." SYS-DD
             NC"　　　　＃" UPON CONS.
     DISPLAY NC"＃　時刻：" SYS-HH "." SYS-MN "." SYS-SS
             NC"　　　　＃" UPON CONS.
     DISPLAY NC"＃　場所：" JYO-F03(1:6) "　　＃"
             UPON CONS.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"   UPON CONS.
*ファイルのクローズ
     CLOSE   HJYOKEN.
*プログラム終了
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
