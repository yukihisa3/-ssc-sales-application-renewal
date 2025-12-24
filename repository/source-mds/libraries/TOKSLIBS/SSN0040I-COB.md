# SSN0040I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSN0040I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理・内部統制対応　　　　　　*
*    モジュール名　　　　：　発注承認入力　　　　　　　　　　　*
*    作成日／更新日　　　：　2008/08/08 -                      *
*    作成者／更新者　　　：　ＮＡＶ　武井　　　　　　　　　　　*
*    処理概要　　　　　　：　発注データに対する承認処理を行う。*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSN0040I.
 AUTHOR.                NAV TAKEI.
 DATE-WRITTEN.          08/08/08.
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
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 発注ファイル >>--*
     SELECT   HACHEDF   ASSIGN         DA-01-VI-HACHEDL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HAC-F99   HAC-F36
                        STATUS         HACHEDF-ST.
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSN00401  OF        XMDLIB.
*----<< 発注ファイル >>--*
 FD  HACHEDF            LABEL     RECORD   IS   STANDARD.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HAC       PREFIX.
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01).
 01  TAN-INV-FLG        PIC  X(01)  VALUE  SPACE.
 01  COUNTERS.
     03  READ-CNT       PIC  9(07)  VALUE  ZERO.
     03  UPD-CNT        PIC  9(07)  VALUE  ZERO.
     03  SKP-CNT        PIC  9(07)  VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HACHEDF-ST        PIC  X(02).
 01  HTANMS-ST         PIC  X(02).
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
** 画面表示用日付
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
     03  SYS-TIMEW1     PIC  9(02).
     03  SYS-TIMEW2     PIC  9(02).
     03  SYS-TIMEW3     PIC  9(02).
     03  FILLER         PIC  X(02).
** 画面表示用時刻
 01  WK-SYSTIME.
     03  WK-TIME-HH     PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-TIME-MM     PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-TIME-SS     PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  FILLER                         PIC  X(10).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF09           PIC  X(04)     VALUE     "F009".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_再入力".
*
 01  MSG-AREA.
     03  MSG01     PIC  N(30)  VALUE
                   NC"ＰＦキーが違います".
     03  MSG02     PIC  N(30)  VALUE
                   NC"承認担当者を入力して下さい".
     03  MSG03     PIC  N(30)  VALUE
                   NC"担当者マスタ未登録です".
     03  MSG04     PIC  N(30)  VALUE
                   NC"承認権限がありません".
     03  MSG05     PIC  N(30)  VALUE
                   NC"入力担当者を入力して下さい".
     03  MSG06     PIC  N(30)  VALUE
                   NC"入力日範囲エラーです".
     03  MSG07     PIC  N(30)  VALUE
                   NC"取引先範囲エラーです".
     03  MSG08     PIC  N(30)  VALUE
                   NC"伝票範囲エラーです".
     03  MSG09     PIC  N(30)  VALUE
                   NC"承認権限担当者です".
*--<< WK    AREA >>-*
***
 01  WK-TAN-AREA.
     03  WK-TAN-F02               PIC  X(02).
     03  WK-TAN-F05               PIC  X(01).
     03  WK-TAN-F06               PIC  X(01).
*
 01  WK-HANI-AREA.
     03  WK-INDAY1                PIC  9(08).
     03  WK-INDAY2                PIC  9(08).

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
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                    SECTION.
 01  LINK-M-TAN             PIC   X(02).
 01  LINK-M-BUMON           PIC   X(04).
****************************************************************
 PROCEDURE          DIVISION   USING  LINK-M-TAN LINK-M-BUMON.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0040I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HACHEDF   HTANMS    DSPFILE.
     STOP     RUN.
*----<< 発注ファイル >>--*
 HACHEDF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACHEDF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0040I SHTDENF ERROR " HACHEDF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HACHEDF   HTANMS    DSPFILE.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0040I HTANMS ERROR " HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HACHEDF   HTANMS    DSPFILE.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
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
     DISPLAY  "*** SSN0040I  START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       HACHEDF.
     OPEN     INPUT     HTANMS.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
     DISPLAY  "LINK-TAN="  LINK-M-TAN  UPON  CONS.
     DISPLAY  "LINK-BUMON="  LINK-M-BUMON  UPON  CONS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｺｳｼﾝ >>-*
     PERFORM  240-UPDATE     UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    HACHEDF.
     CLOSE    HTANMS.
*
     DISPLAY  "**  READ COUNT="   READ-CNT   UPON  CONS.
     DISPLAY  "**  SKIP COUNT="   SKP-CNT    UPON  CONS.
     DISPLAY  "**  UPD  COUNT="   UPD-CNT    UPON  CONS.
*
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSN0040I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSN00401.
*
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW1          TO   WK-TIME-HH.
     MOVE     SYS-TIMEW2          TO   WK-TIME-MM.
     MOVE     SYS-TIMEW3          TO   WK-TIME-SS.
     MOVE     WK-SYSTIME          TO   STIME.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSN00401"     TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾀﾝﾄｳｼｬ   ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "TANCD"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              IF  TANCD  =  SPACE
                  MOVE          SPACE          TO   TANNM
                  MOVE          MSG02          TO   ERRMSG
                ELSE
                  MOVE  TANCD     TO   TAN-F02
                  PERFORM  900-TAN-READ
                  IF  TAN-INV-FLG = "Y"
                      MOVE      SPACE          TO   TANNM
                      MOVE      MSG03          TO   ERRMSG
                  ELSE
                    IF  TAN-F06  =  "1"  OR  "2"  OR  "3"
                      MOVE      TAN-F03    TO   TANNM
                      MOVE      2              TO   GR-NO
                      MOVE      TAN-F02        TO   WK-TAN-F02
                      MOVE      TAN-F05        TO   WK-TAN-F05
                      MOVE      TAN-F06        TO   WK-TAN-F06
                    ELSE
                          MOVE      TAN-F03    TO   TANNM
                          MOVE      MSG04      TO   ERRMSG
                    END-IF
                  END-IF
              END-IF
         WHEN OTHER
              MOVE  MSG01       TO   ERRMSG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ2                            *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
**
              PERFORM   221-INP-SET
              IF  ERRMSG  =  SPACE
                   MOVE             9   TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE   MSG01                     TO   ERRMSG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      範囲設定                                    *
*--------------------------------------------------------------*
 221-INP-SET            SECTION.
     MOVE     "221-INP-SET"  TO   S-NAME.
*
     IF  INDAY1  IS  NOT  NUMERIC
         MOVE  ALL  "0"     TO   INDAY1
     END-IF.
     IF  INDAY2  IS  NOT  NUMERIC
         MOVE  ALL  "9"     TO   INDAY2
     END-IF.
     IF  INDAY1  >  INDAY2
         IF  ERRMSG  =  SPACE
             MOVE   MSG06    TO    ERRMSG
         END-IF
     END-IF.
*
**
     IF  ERRMSG  NOT  =  SPACE
         GO  TO   221-INP-SET-EXIT
     END-IF.
** 日付暦日チェック
     IF  INDAY1  =  ZERO
         GO  TO   221-INP-010C
     END-IF.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     INDAY1(3:6)  TO     LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET NOT  =    ZERO
         IF  ERRMSG  =  SPACE
             MOVE  MSG06     TO   ERRMSG
         END-IF
     END-IF.
*
 221-INP-010C.
     IF  INDAY2  =  ALL "9"
         GO  TO  221-INP-020C
     END-IF.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     INDAY2(3:6)  TO     LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET NOT  =    ZERO
         IF  ERRMSG  =  SPACE
             MOVE  MSG06     TO   ERRMSG
         END-IF
     END-IF.
*
 221-INP-020C.
     MOVE     INDAY1         TO   WK-INDAY1.
     MOVE     INDAY2         TO   WK-INDAY2.
 221-INP-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KAKNIN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE   MSG01        TO   ERRMSG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      承認更新                                    *
*--------------------------------------------------------------*
 240-UPDATE             SECTION.
     MOVE     "240-UPDATE"   TO   S-NAME.
*
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*
     MOVE     ZERO           TO   HAC-F99.
     MOVE     LOW-VALUE      TO   HAC-F36.
**
     IF       INDAY1    IS   NUMERIC
              MOVE      INDAY1    TO   HAC-F99
     END-IF.
**
     PERFORM  900-HAC-START-READ.
     PERFORM  241-HAC-UPDATE
                        UNTIL     NEW  =    HIGH-VALUE.
*
     MOVE     0              TO   GR-NO.
 240-UPDATE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｺｳｼﾝ                                        *
*--------------------------------------------------------------*
 241-HAC-UPDATE         SECTION.
     MOVE    "241-HAC-UPDATE"     TO   S-NAME.
     IF       NEW       =    HIGH-VALUE
              GO  TO    241-HAC-UPDATE-EXIT
     END-IF.
** 権限１
     IF       HAC-F39   =  "1"  OR  "2"  OR  "3"
              ADD       1   TO    SKP-CNT
              GO  TO   241-HAC-010S
     END-IF.
** 承認者
     IF       HAC-F36   NOT =  SPACE
              ADD       1   TO    SKP-CNT
              GO  TO   241-HAC-010S
     END-IF.
*
     PERFORM   2411-MEIS01-REWRITE.
 241-HAC-010S.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-HAC-READ
     END-IF.
 241-HAC-UPDATE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ REWRITE                                 *
*--------------------------------------------------------------*
 2411-MEIS01-REWRITE    SECTION.
     MOVE    "2411-MEIS01-REWRITE"  TO S-NAME.
*
     MOVE     WK-TAN-F02     TO   HAC-F36.
     MOVE     WK-TAN-F05     TO   HAC-F38.
     MOVE     WK-TAN-F06     TO   HAC-F39.
*
     MOVE     SYS-DATEW      TO   HAC-F37.
*
     REWRITE  HAC-REC.
     ADD      1              TO   UPD-CNT.
 2411-MEIS01-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   PFKGID
     ELSE
              MOVE      GUIDE02   TO   PFKGID
     END-IF.
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW1          TO   WK-TIME-HH.
     MOVE     SYS-TIMEW2          TO   WK-TIME-MM.
     MOVE     SYS-TIMEW3          TO   WK-TIME-SS.
     MOVE     WK-SYSTIME          TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
     IF    ERRMSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   ERRMSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
**** MOVE     WK-SYSYMD           TO   SDATE.
**** ACCEPT   SYS-TIME2      FROM TIME.
**** MOVE     SYS-TIMEW1          TO   WK-TIME-HH.
**** MOVE     SYS-TIMEW2          TO   WK-TIME-MM.
**** MOVE     SYS-TIMEW3          TO   WK-TIME-SS.
**** MOVE     WK-SYSTIME          TO   STIME.
****
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSN00401.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    発注ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-HAC-START-READ     SECTION.
     MOVE     "900-HAC-START-READ"     TO   S-NAME.
     START    HACHEDF   KEY  >=   HAC-F99   HAC-F36
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
       DISPLAY  "START-READ OK"  UPON  CONS
              PERFORM   900-HAC-READ
     END-IF.
 900-HAC-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    発注ファイル　 READ                          *
*--------------------------------------------------------------*
 900-HAC-READ           SECTION.
     MOVE     "900-HAC-READ"      TO   S-NAME.
 900-HAC-010S.
     READ     HACHEDF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-HAC-READ-EXIT
     END-READ.
     ADD      1    TO   READ-CNT.
** 修正日
     IF       HAC-F99 <  WK-INDAY1

              GO  TO    900-HAC-010S
     END-IF.
     IF       HAC-F99 >  WK-INDAY2
              MOVE      HIGH-VALUE     TO   NEW
              GO  TO    900-HAC-READ-EXIT
     END-IF.
*
 900-HAC-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    担当者マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TAN-READ           SECTION.
     MOVE     "900-TAN-READ"      TO   S-NAME.
     MOVE     LINK-M-BUMON        TO   TAN-F01.
     MOVE     SPACE               TO   TAN-INV-FLG.
     READ     HTANMS    INVALID
              MOVE      SPACE          TO   TAN-F03
              MOVE      "Y"       TO   TAN-INV-FLG
     END-READ.
 900-TAN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
