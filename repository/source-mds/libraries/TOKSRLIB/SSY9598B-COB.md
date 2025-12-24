# SSY9598B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9598B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　　  *
*    モジュール名　　　　：　ＹＯＤＯＲＤＰＦ　　　　コンバート*
*    作成日／作成者　　　：　2023/05/10 INOUE                  *
*    処理概要　　　　　　：　旧レイアウト→新レイアウト　　　　*
*                            ※一時使用　　　　　　　　　      *
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9598B.
*
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/05/10.
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
*----<< 旧.ファイル >>--*
     SELECT   OLDORDPF  ASSIGN         DA-01-VS-OLDORDPF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         OLDORDPF-ST.
*----<< 新.ファイル >>--*
     SELECT   YODORDPF  ASSIGN         DA-01-VS-YODORDPF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         YODORDPF-ST.
*----<< 参照ファイル >>--*
     SELECT   YODMEIL1  ASSIGN         DA-01-VI-YODMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F01
                        STATUS         YODMEIL1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 旧.ファイル >>--*
 FD  OLDORDPF            LABEL     RECORD   IS   STANDARD.
     COPY     OLDORDPF   OF        XFDLIB
              JOINING    OLD       PREFIX.
*----<< 新.ファイル >>--*
 FD  YODORDPF            LABEL     RECORD   IS   STANDARD.
     COPY     YODORDPF   OF        XFDLIB
              JOINING    NEW       PREFIX.
*----<< 参照ファイル >>--*
 FD  YODMEIL1            LABEL     RECORD   IS   STANDARD.
     COPY     YODMEIL1   OF        XFDLIB
              JOINING    MEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  OLDORDPF-ST       PIC  X(02).
 01  YODORDPF-ST       PIC  X(02).
 01  YODMEIL1-ST       PIC  X(02).
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
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
 01  WK-YODOBASI          PIC  9(08)  VALUE  ZERO.
 01  RD-CNT               PIC  9(08)  VALUE  ZERO.
 01  WRT-CNT              PIC  9(08)  VALUE  ZERO.
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
****************************************************************
 PROCEDURE  DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 旧.ファイル >>--*
 OLDORDPF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OLDORDPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9598B OLDORDPF ERROR " OLDORDPF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 新.ファイル >>--*
 YODORDPF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODORDPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9598B YODORDPF ERROR " YODORDPF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 参照ファイル >>--*
 YODMEIL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE     YODMEIL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9598B YODMEIL1 ERROR " YODMEIL1-ST " "
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
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*
     MOVE    "100-INIT-RTN"       TO   S-NAME.
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
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY9598B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     OLDORDPF.
     OPEN     INPUT     YODMEIL1.
     OPEN     OUTPUT    YODORDPF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
 MAIN-01.
     READ     OLDORDPF
       AT END
              GO TO  200-MAIN-RTN-EXIT
     END-READ.
     ADD      1        TO  RD-CNT.
*
     MOVE     SPACE    TO  NEW-REC.
     INITIALIZE            NEW-REC.
     MOVE     OLD-REC  TO  NEW-REC.
     MOVE     NEW-F08  TO  MEI-F01.
     READ     YODMEIL1
          INVALID
              DISPLAY NC"ＪＡＮＣＤなし　" NEW-F08 UPON CONS
          NOT INVALID
              MOVE  MEI-F02  TO  NEW-F24
     END-READ.
*    MOVE     ZERO     TO  NEW-F29.
*    MOVE     ZERO     TO  NEW-F30.
*    MOVE     ZERO     TO  NEW-F31.
*    MOVE     ZERO     TO  NEW-F32.
*    MOVE     ZERO     TO  NEW-F33.
*    MOVE     ZERO     TO  NEW-F34.
*    MOVE     ZERO     TO  NEW-F35.
*    MOVE     ZERO     TO  NEW-F37.
*
     WRITE    NEW-REC.
     ADD      1        TO  WRT-CNT.
*
     GO  TO   MAIN-01.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    OLDORDPF.
     CLOSE    YODORDPF.
     CLOSE    YODMEIL1.
*
     DISPLAY  "RD-CNT=" RD-CNT  UPON CONS.
     DISPLAY  "WT-CNT=" WRT-CNT UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY9598B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
