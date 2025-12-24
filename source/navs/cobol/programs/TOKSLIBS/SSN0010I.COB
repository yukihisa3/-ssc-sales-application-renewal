****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理・内部統制対応　　　　　　*
*    モジュール名　　　　：　手書伝票承認入力　　　　　　　　　*
*    作成日／更新日　　　：　2008/08/06 - 08/07                *
*    作成者／更新者　　　：　ＮＡＶ　武井　　　　　　　　　　　*
*    処理概要　　　　　　：　手書伝票データの承認処理を行う。　*
*    更新日／更新者　　　：　2011/10/11 /YOSHIDA.M             *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSN0010I.
 AUTHOR.                NAV TAKEI.
 DATE-WRITTEN.          08/08/06.
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
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLG
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F60   DEN-F63
                                       DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
***2011.10.11 ST
                                       DEN-F07   DEN-F112
***2011.10.11 EN
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
*----<< 伝票承認ファイル >>--*
     SELECT   DENSYOF   ASSIGN         DA-01-VI-DENSYOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DES-F01   DES-F02
                                       DES-F03   DES-F04
                        STATUS         DENSYOF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSN00101  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< 伝票承認ファイル >>--*
 FD  DENSYOF            LABEL RECORD   IS   STANDARD.
     COPY     DENSYOF   OF        XFDLIB
              JOINING   DES       PREFIX.
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
     03  DEN-CNT        PIC  9(07)  VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST         PIC  X(02).
 01  HTANMS-ST          PIC  X(02).
 01  DENSYOF-ST         PIC  X(02).
*
*----<< 伝票番号退避 >>--*
 01  WK-DEN-F02         PIC  9(09)  VALUE  ZERO.
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
     03  WK-TORIC1                PIC  9(08).
     03  WK-TORIC2                PIC  9(08).
     03  WK-DENNO1                PIC  9(09).
     03  WK-DENNO2                PIC  9(09).

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
 01  LINK-M-NYURYOKUHI      PIC   9(08).
 01  LINK-M-STIME           PIC   9(06).
****************************************************************
 PROCEDURE          DIVISION   USING  LINK-M-TAN  LINK-M-BUMON
                               LINK-M-NYURYOKUHI  LINK-M-STIME.
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
     DISPLAY  "### SSN0010I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTANMS    DSPFILE  DENSYOF.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0010I SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTANMS    DSPFILE  DENSYOF.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0010I HTANMS ERROR " HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTANMS    DSPFILE  DENSYOF.
     STOP     RUN.
*----<< 伝票承認ファイル >>--*
 DENSYOF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DENSYOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSN0010I DENSYOF ERROR " DENSYOF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTANMS    DSPFILE  DENSYOF.
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
*自動出力の為のキー情報をセット
     MOVE     SYS-DATEW      TO        LINK-M-NYURYOKUHI.
     MOVE     SYS-TIME(1:6)  TO        LINK-M-STIME
*
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
     DISPLAY  "*** SSN0010I  START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       SHTDENF  DENSYOF.
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
     CLOSE    SHTDENF.
     CLOSE    HTANMS.
*
     DISPLAY  "**  READ COUNT="   READ-CNT   UPON  CONS.
     DISPLAY  "**  SKIP COUNT="   SKP-CNT    UPON  CONS.
     DISPLAY  "**  UPD  COUNT="   UPD-CNT    UPON  CONS.
*
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSN0010I END *** "
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
     MOVE     SPACE          TO   FSN00101.
*
***  MOVE    "SSN0010I"      TO   PGID.
***  MOVE    "FSN00101"      TO   FORM.
*
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW1          TO   WK-TIME-HH.
     MOVE     SYS-TIMEW2          TO   WK-TIME-MM.
     MOVE     SYS-TIMEW3          TO   WK-TIME-SS.
     MOVE     WK-SYSTIME          TO   STIME.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSN00101"     TO   DSP-FMT.
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
              IF  INTCD  =  SPACE
                  MOVE          SPACE          TO   INTNM
                  MOVE          MSG05          TO   ERRMSG
                ELSE
                  MOVE  INTCD     TO   TAN-F02
                  PERFORM  900-TAN-READ
                  IF  TAN-INV-FLG = "Y"
                      MOVE      SPACE          TO   INTNM
                      MOVE      MSG03          TO   ERRMSG
                    ELSE
                      MOVE      TAN-F03        TO   INTNM
                      PERFORM   221-INP-SET
                      IF  ERRMSG  =  SPACE
                          MOVE             9   TO   GR-NO
                      END-IF
                  END-IF
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
     IF  TORIC1  IS  NOT  NUMERIC
         MOVE  ALL "0"       TO  TORIC1
     END-IF.
     IF  TORIC2  IS  NOT  NUMERIC
         MOVE  ALL "9"       TO  TORIC2
     END-IF.
     IF  TORIC1  >  TORIC2
         IF  ERRMSG  =  SPACE
             MOVE   MSG07    TO    ERRMSG
         END-IF
     END-IF.
*
     IF  DENNO1  IS  NOT  NUMERIC
         MOVE  ALL   "0"           TO    DENNO1
     END-IF.
     IF  DENNO2  IS  NOT  NUMERIC
         MOVE  ALL   "9"           TO    DENNO2
     END-IF.
     IF  DENNO1  >  DENNO2
         IF  ERRMSG  =  SPACE
             MOVE   MSG08    TO    ERRMSG
         END-IF
     END-IF.
**
     IF  ERRMSG  NOT  =  SPACE
         GO  TO   221-INP-SET-EXIT
     END-IF.
*
     MOVE     INDAY1         TO   WK-INDAY1.
     MOVE     INDAY2         TO   WK-INDAY2.
     MOVE     TORIC1         TO   WK-TORIC1.
     MOVE     TORIC2         TO   WK-TORIC2.
     MOVE     DENNO1         TO   WK-DENNO1.
     MOVE     DENNO2         TO   WK-DENNO2.
*
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
***2011.10.11 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.11 EN
     MOVE     ZERO           TO   UPD-CNT   DEN-CNT.
     MOVE     ZERO           TO   WK-DEN-F02.
     MOVE     ZERO           TO   DEN-F01.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F051.
     MOVE     ZERO           TO   DEN-F03.
     MOVE     ZERO           TO   DEN-F60.
     MOVE     ZERO           TO   DEN-F63.
     IF       TORIC1    IS   NUMERIC
              MOVE      TORIC1    TO   DEN-F01
     END-IF.
     IF       DENNO1    IS   NUMERIC
              MOVE      DENNO1    TO   DEN-F02
     END-IF.
     IF       INDAY1    IS   NUMERIC
              MOVE      INDAY1    TO   DEN-F63
     END-IF.
     MOVE     INTCD          TO   DEN-F60.
     PERFORM  900-DEN-START-READ.
     PERFORM  241-DEN-UPDATE
                        UNTIL     NEW  =    HIGH-VALUE.
*
     PERFORM  DENSYOF-WT-SEC.
*
     MOVE     0              TO   GR-NO.
 240-UPDATE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｺｳｼﾝ                                        *
*--------------------------------------------------------------*
 241-DEN-UPDATE         SECTION.
     MOVE    "241-DEN-UPDATE"     TO   S-NAME.
     IF       NEW       =    HIGH-VALUE
              GO  TO    241-DEN-UPDATE-EXIT
     END-IF.
** オンライン
     IF       DEN-F274  =  1
              ADD       1   TO    SKP-CNT
              GO  TO   241-DEN-010S
     END-IF.
** 権限１
     IF       DEN-F66   =  "1"  OR  "2"  OR  "3"
              ADD       1   TO    SKP-CNT
              GO  TO   241-DEN-010S
     END-IF.
** 承認者
     IF       DEN-F61   NOT =  SPACE
              ADD       1   TO    SKP-CNT
              GO  TO   241-DEN-010S
     END-IF.
*
     PERFORM   2411-MEIS01-REWRITE.
 241-DEN-010S.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 241-DEN-UPDATE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ REWRITE                                 *
*--------------------------------------------------------------*
 2411-MEIS01-REWRITE    SECTION.
     MOVE    "2411-MEIS01-REWRITE"  TO S-NAME.
*
     MOVE     WK-TAN-F02     TO   DEN-F61.
     MOVE     WK-TAN-F05     TO   DEN-F65.
     MOVE     WK-TAN-F06     TO   DEN-F66.
*
     MOVE     SYS-DATEW      TO   DEN-F64.
*
     REWRITE  DEN-REC.
     ADD      1              TO   UPD-CNT.
*伝票枚数カウント
     IF       DEN-F02  NOT =  WK-DEN-F02
              ADD    1       TO   DEN-CNT
              MOVE DEN-F02   TO   WK-DEN-F02
     END-IF.
*
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
     WRITE    FSN00101.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     MOVE     "900-DEN-START-READ"     TO   S-NAME.
***2011.10.11 ST
***  START    SHTDENF   KEY  >=   DEN-F60   DEN-F63    DEN-F01
***                     DEN-F02   DEN-F04   DEN-F051   DEN-F03
     START    SHTDENF   KEY  >=   DEN-F60   DEN-F63    DEN-F01
                        DEN-F02   DEN-F04   DEN-F051   DEN-F07
                        DEN-F112  DEN-F03
***2011.10.11 EN
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
       DISPLAY  "START-READ OK"  UPON  CONS
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
 900-DEN-010S.
     READ     SHTDENF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
     ADD      1    TO   READ-CNT.
** 入力担当者
     IF       INTCD  NOT  =  DEN-F60
              MOVE      HIGH-VALUE     TO   NEW
              GO  TO    900-DEN-READ-EXIT
     END-IF.
**
     IF       DEN-F63 <  WK-INDAY1

              GO  TO    900-DEN-010S
     END-IF.
     IF       DEN-F63 >  WK-INDAY2
              MOVE      HIGH-VALUE     TO   NEW
              GO  TO    900-DEN-READ-EXIT
     END-IF.
**
     IF       DEN-F01 <  WK-TORIC1

              GO  TO    900-DEN-010S
     END-IF.
     IF       DEN-F01 >  WK-TORIC2
              MOVE      HIGH-VALUE     TO   NEW
              GO  TO    900-DEN-READ-EXIT
     END-IF.
**
     IF       DEN-F02  <  WK-DENNO1  OR
              DEN-F02  >  WK-DENNO2
              GO  TO    900-DEN-010S
     END-IF.
*
 900-DEN-READ-EXIT.
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
*--------------------------------------------------------------*
*    LEVEL ALL    伝票承認ファイル作成
*--------------------------------------------------------------*
 DENSYOF-WT-SEC         SECTION.
*
     MOVE     "DENSYOF-WT-SEC"    TO   S-NAME.
*項目初期化
     MOVE     SPACE               TO   DES-REC.
     INITIALIZE                        DES-REC.
*承認日
     MOVE     SYS-DATEW           TO   DES-F01.
*入力担当者
     MOVE     INTCD               TO   DES-F02.
*承認時刻
     MOVE     SYS-TIME(1:6)       TO   DES-F03.
*承認担当者
     MOVE     TANCD               TO   DES-F04.
*入力日開始
     MOVE     INDAY1              TO   DES-F05.
*入力日終了
     MOVE     INDAY2              TO   DES-F06.
*取引先開始
     MOVE     TORIC1              TO   DES-F07.
*取引先終了
     MOVE     TORIC2              TO   DES-F08.
*伝票番号開始
     MOVE     DENNO1              TO   DES-F09.
*伝票番号終了
     MOVE     DENNO2              TO   DES-F10.
*明細件数
     MOVE     UPD-CNT             TO   DES-F11.
*伝票枚数
     MOVE     DEN-CNT             TO   DES-F12.
*登録担当者
     MOVE     TANCD               TO   DES-F94.
*登録日付
     MOVE     SYS-DATEW           TO   DES-F95.
*登録時刻
     MOVE     SYS-TIME(1:6)       TO   DES-F96.
*更新担当者
     MOVE     TANCD               TO   DES-F97.
*更新日付
     MOVE     SYS-DATEW           TO   DES-F98.
*更新時刻
     MOVE     SYS-TIME(1:6)       TO   DES-F99.
*伝票承認ファイル更新
     WRITE    DES-REC.
*
 DENSYOF-WT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
