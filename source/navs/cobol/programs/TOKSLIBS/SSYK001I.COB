****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　
*    業務名　　　　　　　：　出荷明細表
*    モジュール名　　　　：　出荷明細表範囲入力
*    作成日／更新日　　　：　99/09/20
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　
*    処理概要　　　　　　：　画面より、出荷明細表発行指示を
*                            入力し、チェック後パラメタに渡
*                            す。
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSYK001I.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/20.
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
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                   *> 2011/10/07,S  S.I/NAV
                   *>     RECORD    KEY  DEN-F46   DEN-F47
                   *>                    DEN-F01   DEN-F48
                   *>                    DEN-F02   DEN-F04
                   *>                    DEN-F051  DEN-F03
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                   *> 2011/10/07,E  S.I/NAV
                        STATUS         SHTDENF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSYK0011  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
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
 01  WK-NOUYMD.
     03  WK-NOUYY       PIC  9(04)     VALUE  ZERO.
     03  WK-NOUMM       PIC  9(02)     VALUE  ZERO.
     03  WK-NOUDD       PIC  9(02)     VALUE  ZERO.
 01  WK-NOUYMD-R        REDEFINES      WK-NOUYMD.
     03  WK-NOUHIN      PIC  9(08).
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
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"バッチＮｏを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"バッチＮｏに誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(20)  VALUE
              NC"売上伝票データに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"出力順が違います。".
     03  MSG07               PIC  N(20)  VALUE
              NC"範囲指定が違います。".
     03  MSG08               PIC  N(20)  VALUE
              NC"対象倉庫が違います。".
     03  MSG09               PIC  N(20)  VALUE
              NC"納品日が違います。".
     03  MSG10               PIC  N(20)  VALUE
              NC"受信日が違います。".
     03  MSG11               PIC  N(20)  VALUE
              NC"出力パターンが違います。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       11.
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
 LINKAGE                SECTION.
 01  PARA-OUTNO             PIC   9(01).
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUHIN            PIC   9(08).
 01  PARA-STEN              PIC   9(05).
 01  PARA-ETEN              PIC   9(05).
 01  PARA-SBUMON            PIC   X(04).
 01  PARA-EBUMON            PIC   X(04).
 01  PARA-STANA             PIC   X(06).
 01  PARA-ETANA             PIC   X(06).
 01  PARA-PATAN             PIC   9(01).
 01  PARA-DSOKO             PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-OUTNO
                                       PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUHIN
                                       PARA-STEN
                                       PARA-ETEN
                                       PARA-SBUMON
                                       PARA-EBUMON
                                       PARA-STANA
                                       PARA-ETANA
                                       PARA-PATAN
                                       PARA-DSOKO.
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
     DISPLAY  "### SSYK001I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSYK001I SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSYK001I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSYK001I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS1    DSPFILE.
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
     DISPLAY  "*** SSYK001I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｼｭﾂﾘｮｸｼﾞｭﾝ  ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSYK001I END *** "
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
     MOVE     SPACE          TO   FSYK0011.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-BODY2-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSYK001I"      TO   PGID.
     MOVE    "FSYK0011"      TO   FORM.
     MOVE     WK-SYSYMD      TO   SYSYMD.
*
*倉庫コードパラメタより倉庫コード入力チェック
     DISPLAY "PARA-SOKO  = " PARA-SOKO  UPON CONS.
     DISPLAY "PARA-DSOKO = " PARA-DSOKO UPON CONS.
     IF       PARA-DSOKO  =  "01"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01 SOKO
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKONM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKONM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
     END-IF.
*出力パターンセット
     MOVE     1              TO   PATAN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSYK0011"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "HEAD01"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( JDATE     =    ZERO ) AND
            ( JTIME     =    ZERO ) AND
            ( TORICD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
              GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
*    受信日付チェック
     IF  JDATE     NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      JDATE     TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              MOVE      10        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*    取引先チェック
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      4    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-READ
     END-IF.
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   DEN-REC
         INITIALIZE                    DEN-REC
         MOVE      JDATE          TO   DEN-F46
         MOVE      JTIME          TO   DEN-F47
         MOVE      TORICD         TO   DEN-F01
         MOVE      SPACE          TO   DEN-F48
    *> 2011/10/07,S  S.I/NAV
    *>     START     SHTDENF   KEY  >=   DEN-F46   DEN-F47
    *>                                   DEN-F01   DEN-F48
    *>                                   DEN-F02   DEN-F04
    *>                                   DEN-F051  DEN-F03
         START     SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
    *> 2011/10/07,E  S.I/NAV
              INVALID   KEY
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   GO   TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                   READ      SHTDENF
                     AT END
                        MOVE      5    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JTIME
                        MOVE     "R"   TO   EDIT-OPTION OF TORICD
                        GO   TO   220-GRP01-CHECK-EXIT
                     NOT AT END
                        IF ( JDATE     =    DEN-F46 ) AND
                           ( JTIME     =    DEN-F47 ) AND
                           ( TORICD    =    DEN-F01 )
                             CONTINUE
                        ELSE
                             MOVE      5    TO   ERR-FLG
                             MOVE "C"  TO   EDIT-CURSOR OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JTIME
                             MOVE "R"  TO   EDIT-OPTION OF TORICD
                             GO   TO   220-GRP01-CHECK-EXIT
                        END-IF
                   END-READ
         END-START
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｼｭﾂﾘｮｸｼﾞｭﾝ ﾆｭｳﾘｮｸ                           *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "BODY00"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              IF ( OUTNO     IS NUMERIC       ) AND
                 ( OUTNO     =    1    OR   2 )
                   CONTINUE
              ELSE
                   MOVE      6    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF OUTNO
                   MOVE     "R"   TO   EDIT-OPTION OF OUTNO
              END-IF
              IF ( PATAN     IS NUMERIC       ) AND
                 ( PATAN     =    1    OR   2 )
                   CONTINUE
              ELSE
                   IF  ERR-FLG = ZERO
                       MOVE      11   TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF PATAN
                   MOVE     "R"   TO   EDIT-OPTION OF PATAN
              END-IF
              IF   ERR-FLG = ZERO
                   MOVE      3    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     IF        OUTNO    =    1
               MOVE    "BODY01"   TO   WK-GRP
     ELSE
               MOVE    "BODY02"   TO   WK-GRP
     END-IF.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      2         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BODY2-RTN
              PERFORM   220-GRP03-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1                   TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3                                                  *
*--------------------------------------------------------------*
 220-GRP03-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    倉庫コードチェック
*****IF     ( SOKO      IS   NUMERIC   )   AND
*****       ( SOKO      NOT =     ZERO )
     IF     ( SOKO      NOT =  SPACE   )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        MOVE      8    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
*
                        PERFORM   220-SONZAI-CHECK
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKONM
     END-IF.
*    納品日チェック
     IF    (( NOUYY     IS NUMERIC     ) AND
            ( NOUYY     >         ZERO )     ) OR
           (( NOUMM     IS NUMERIC     ) AND
            ( NOUMM     >         ZERO )     ) OR
           (( NOUDD     IS NUMERIC     ) AND
            ( NOUDD     >         ZERO )     )
              MOVE      NOUYY     TO        WK-NOUYY
              MOVE      NOUMM     TO        WK-NOUMM
              MOVE      NOUDD     TO        WK-NOUDD
              MOVE      WK-NOUYMD TO        WK-NOUHIN
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      WK-NOUHIN TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUYY
                   MOVE     "R"   TO   EDIT-OPTION OF NOUYY
                   MOVE     "R"   TO   EDIT-OPTION OF NOUMM
                   MOVE     "R"   TO   EDIT-OPTION OF NOUDD
                   GO   TO   220-GRP03-CHECK-EXIT
              END-IF
     END-IF.
*    店舗
     IF     ( STEN      IS NUMERIC     AND
              ETEN      IS NUMERIC     AND
              STEN      >    ETEN   )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF STEN
              MOVE     "R"   TO   EDIT-OPTION OF STEN
              MOVE     "R"   TO   EDIT-OPTION OF ETEN
     END-IF.
*    部門
     IF     ( EBUMON    NOT =     SPACE ) AND
            ( SBUMON    >    EBUMON     )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SBUMON
              MOVE     "R"   TO   EDIT-OPTION OF SBUMON
              MOVE     "R"   TO   EDIT-OPTION OF EBUMON
     END-IF.
*    _番
     IF     ( ETANA     NOT =    SPACE ) AND
            ( STANA     >    ETANA     )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF STANA
              MOVE     "R"   TO   EDIT-OPTION OF STANA
              MOVE     "R"   TO   EDIT-OPTION OF ETANA
     END-IF.
*
 220-GRP03-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳ ｿﾝｻﾞｲ ﾁｪｯｸ                     *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     JDATE          TO   DEN-F46.
     MOVE     JTIME          TO   DEN-F47.
     MOVE     TORICD         TO   DEN-F01.
     MOVE     SOKO           TO   DEN-F48.
* 2011/10/07,S  S.I/NAV
**     START    SHTDENF   KEY  >=   DEN-F46   DEN-F47
**                                  DEN-F01   DEN-F48
**                                  DEN-F02   DEN-F04
**                                  DEN-F051  DEN-F03
     START    SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
* 2011/10/07,E  S.I/NAV
         INVALID   KEY
              MOVE      5    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF SOKO
              MOVE     "R"        TO   EDIT-OPTION OF SOKO
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
              READ      SHTDENF
                AT END
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                   MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
                   IF ( JDATE     =    DEN-F46 ) AND
                      ( JTIME     =    DEN-F47 ) AND
                      ( TORICD    =    DEN-F01 ) AND
                      ( SOKO      =    DEN-F48 )
                        CONTINUE
                   ELSE
                        MOVE      5    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF SOKO
                   MOVE "R"  TO   EDIT-OPTION OF SOKO
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      3         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     OUTNO               TO   PARA-OUTNO.
     MOVE     JDATE               TO   PARA-JDATE.
     MOVE     JTIME               TO   PARA-JTIME.
     MOVE     TORICD              TO   PARA-TORICD.
*****IF       SOKO      IS NOT NUMERIC
     IF       SOKO      =     SPACE
              MOVE      SPACE     TO   PARA-SOKO
     ELSE
              MOVE      SOKO      TO   PARA-SOKO
     END-IF.
     MOVE     NOUYY               TO   WK-NOUYY.
     MOVE     NOUMM               TO   WK-NOUMM.
     MOVE     NOUDD               TO   WK-NOUDD.
     MOVE     WK-NOUYMD           TO   WK-NOUHIN.
     MOVE     WK-NOUHIN           TO   PARA-NOUHIN.
     IF       NOUYY     IS NOT NUMERIC
              MOVE      ZERO      TO   PARA-NOUHIN
     END-IF.
*****MOVE     STEN                TO   PARA-STEN.
*## 1999/10/12 T.T START ##*数字以外の場合はゼロをリテラルセット
     IF       STEN  NOT  NUMERIC
              MOVE      ZERO      TO   PARA-STEN
     ELSE
              MOVE      STEN      TO   PARA-STEN
     END-IF.
*****IF       ETEN      =    ZERO
     IF       ETEN      NOT  NUMERIC
     OR       ETEN      =    ZERO
*## 1999/10/12 T.T END   ##*
**************MOVE     "99999"    TO   PARA-ETEN
              MOVE      99999     TO   PARA-ETEN
     ELSE
              MOVE      ETEN      TO   PARA-ETEN
     END-IF.
     MOVE     SBUMON              TO   PARA-SBUMON.
     IF       EBUMON    =    SPACE
              MOVE      HIGH-VALUE     TO   PARA-EBUMON
     ELSE
              MOVE      EBUMON         TO   PARA-EBUMON
     END-IF.
     MOVE     STANA               TO   PARA-STANA.
     IF       ETANA     =    SPACE
              MOVE      HIGH-VALUE     TO   PARA-ETANA
     ELSE
              MOVE      ETANA          TO   PARA-ETANA
     END-IF.
     MOVE     PATAN               TO   PARA-PATAN.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
***2011.11.22 START
***  MOVE     "SCRERE"       TO   DSP-GRP.
     MOVE     "SCREEN"       TO   DSP-GRP.
***2011.11.22 END
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
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
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSYK0011.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF OUTNO.
     MOVE     "M"            TO   EDIT-OPTION OF OUTNO.
     MOVE     " "            TO   EDIT-CURSOR OF PATAN.
     MOVE     "M"            TO   EDIT-OPTION OF PATAN.
*
 CLR-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ２属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY2-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF SOKO
                                  EDIT-CURSOR OF NOUYY
                                  EDIT-CURSOR OF NOUMM
                                  EDIT-CURSOR OF NOUDD
                                  EDIT-CURSOR OF STEN
                                  EDIT-CURSOR OF ETEN
                                  EDIT-CURSOR OF SBUMON
                                  EDIT-CURSOR OF EBUMON
                                  EDIT-CURSOR OF STANA
                                  EDIT-CURSOR OF ETANA.
     MOVE     "M"            TO   EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF NOUYY
                                  EDIT-OPTION OF NOUMM
                                  EDIT-OPTION OF NOUDD
                                  EDIT-OPTION OF STEN
                                  EDIT-OPTION OF ETEN
                                  EDIT-OPTION OF SBUMON
                                  EDIT-OPTION OF EBUMON
                                  EDIT-OPTION OF STANA
                                  EDIT-OPTION OF ETANA.
*
 CLR-BODY2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
