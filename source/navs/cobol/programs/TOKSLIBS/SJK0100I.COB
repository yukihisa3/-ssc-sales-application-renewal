****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品　　　　　　　　　　　*
*    モジュール名　　　　：　受注自動欠品指示画面　　　　　　　*
*    作成日／更新日　　　：　2017/05/23                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    ＯＵＴＰＵＴ　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*                        ：　納品日開始   PARA-NOBIST          *
*                        ：　納品日終了   PARA-NOBIED　　　　　*
*                        ：　欠品区分　   PARA-KEPKBN　　　　　*
*    処理概要　　　　　　：　自動欠品処理の指示を行う　　　　　*
*                        ：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0100I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/05/23.
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
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 出荷制限マスタ >>--*
     SELECT   SYUSGNF   ASSIGN         DA-01-VI-SYUSGNL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SYU-F01
                                       SYU-F02
                                       SYU-F03
                        STATUS         SYUSGNF-ST.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FJK01001  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 出荷制限マスタ >>--*
 FD  SYUSGNF            LABEL RECORD   IS   STANDARD.
     COPY     SYUSGNF   OF        XFDLIB
              JOINING   SYU       PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HTOKMS-ST         PIC  X(02).
 01  SYUSGNF-ST        PIC  X(02).
 01  SHTDENF-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG    PIC  X(01).
 01  SYU-INVALID-FLG    PIC  X(01).
 01  DEN-INVALID-FLG    PIC  X(01).
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
*----<< マスタ取込日 >>--*
 01  WK-MST-TORIKOMI-BI.
     03  TORIKOMI-YYYY  PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  TORIKOMI-MM    PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  TORIKOMI-DD    PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE SPACE.
     03  TORIKOMI-HH    PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  TORIKOMI-MN    PIC  9(02).
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
 01  GUIDE01       PIC  N(40)  VALUE   NC"ＰＦ５：終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"ＰＦ４：取　消　ＰＦ５：終　了　ＰＦ６：項目戻り".
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
              NC"売上伝票ファイルに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"出荷制限マスタに存在しません。".
     03  MSG07               PIC  N(20)  VALUE
              NC"対象データがありません。".
     03  MSG08               PIC  N(20)  VALUE
              NC"既に確定データ作成済みです。".
     03  MSG09               PIC  N(20)  VALUE
              NC"既にデータ送信済みです。".
     03  MSG10               PIC  N(20)  VALUE
              NC"納品日開始を正しく入力して下さい。".
     03  MSG11               PIC  N(20)  VALUE
              NC"取引先ＣＤは、資材ＣＤを入力して下さい。".
     03  MSG12               PIC  N(20)  VALUE
              NC"取引先ＣＤは、植物ＣＤを入力して下さい。".
     03  MSG13               PIC  N(20)  VALUE
              NC"欠品区分は、空白、１、２が入力可能です。".
     03  MSG14               PIC  N(20)  VALUE
              NC"納品日終了を正しく入力して下さい。".
     03  MSG15               PIC  N(20)  VALUE
              NC"開始が終了を超えております。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      15.
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
*欠品メッセージ編集
 01  WK-KEPMSG.
     03  WK-KEPMSG1         PIC   N(20).
     03  WK-KEPMSG2         PIC   N(20).
*
 LINKAGE                SECTION.
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-NOBIS             PIC   9(08).
 01  PARA-NOBIE             PIC   9(08).
 01  PARA-KBN               PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-DATE
                                       PARA-TIME
                                       PARA-TORICD
                                       PARA-NOBIS
                                       PARA-NOBIE
                                       PARA-KBN.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJK0100I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    SYUSGNF    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJK0100I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    SYUSGNF    DSPFILE.
     STOP     RUN.
*----<< 出荷制限マスタ >>--*
 SYUSGNF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGNF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJK0100I SYUSGNF ERROR " SYUSGNF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    SYUSGNF    DSPFILE.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJK0100I SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    SYUSGNF    DSPFILE.
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
     DISPLAY  "*** SJK0100I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     SYUSGNF.
     OPEN     INPUT     SHTDENF.
*
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
     CLOSE    HTOKMS.
     CLOSE    SYUSGNF.
     CLOSE    SHTDENF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJK0100I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"  TO   S-NAME.
     MOVE     SPACE          TO   FJK01001.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SJK0100I"      TO   PGID.
     MOVE    "FJK01001"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FJK01001"     TO   DSP-FMT.
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
     MOVE     SPACE               TO   KEPMSG.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
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
*----<< ﾐﾆｭｳﾘｮｸ CHECK >>--*
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
*
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
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*
*    取引先チェック
     IF  TORICD  NOT  NUMERIC
     AND TORICD  =  ZERO
         IF   ERR-FLG   =    ZERO
              MOVE      4    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         PERFORM 900-TOK-READ
         IF  TOK-INVALID-FLG = "1"
                   MOVE      TOK-F02   TO   TORINM
         ELSE
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-IF
     END-IF.
*    納品日開始チェック
     IF       NOUDTS NOT NUMERIC
              MOVE      ZERO      TO   NOUDTS
     ELSE
              IF   NOUDTS  NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     NOUDTS    TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      10   TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF NOUDTS
                            MOVE  "R"   TO  EDIT-OPTION OF NOUDTS
                   END-IF
              END-IF
     END-IF.
*    納品日終了チェック
     IF       NOUDTE NOT NUMERIC
              MOVE      99999999  TO   NOUDTE
     ELSE
              IF   NOUDTE  =  ZERO  OR  99999999
                   CONTINUE
              ELSE
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     NOUDTE    TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      14   TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF NOUDTE
                            MOVE  "R"   TO  EDIT-OPTION OF NOUDTE
                   END-IF
              END-IF
     END-IF.
*    納品日大小チェック
     IF  NOUDTS  >  NOUDTE
         IF   ERR-FLG   =    ZERO
              MOVE      15   TO   ERR-FLG
         END-IF
         MOVE  "C"   TO  EDIT-CURSOR OF NOUDTS
         MOVE  "R"   TO  EDIT-OPTION OF NOUDTS
         MOVE  "R"   TO  EDIT-OPTION OF NOUDTE
     END-IF.
*    欠品区分チェック
     IF  KEPKBN  =  SPACE  OR  "1"  OR  "2"
         CONTINUE
         EVALUATE   KEPKBN
             WHEN   SPACE
                    MOVE NC"マスタ内容欠品"   TO   KEPMSG
             WHEN   "1"
                    MOVE NC"全　　欠　　品"   TO   KEPMSG
             WHEN   "2"
                    MOVE NC"出荷停止欠品　"   TO   KEPMSG
                    MOVE ZERO                 TO   NOUDTS
                    MOVE 99999999             TO   NOUDTE
         END-EVALUATE
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      13   TO   ERR-FLG
         END-IF
         MOVE  "C"   TO  EDIT-CURSOR OF KEPKBN
         MOVE  "R"   TO  EDIT-OPTION OF KEPKBN
     END-IF.
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   DEN-REC
         INITIALIZE                    DEN-REC
         PERFORM 900-DEN-READ
         IF  DEN-INVALID-FLG = " "
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*----<< 出荷制限マスタ存在チェック >>--*
     IF  ERR-FLG        =    ZERO
     AND KEPKBN         =    SPACE
         MOVE      SPACE          TO   SYU-REC
         INITIALIZE                    SYU-REC
         PERFORM 900-SYU-READ
         IF  SYU-INVALID-FLG = " "
                   MOVE      6    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   GO   TO   220-GRP01-CHECK-EXIT
           ELSE
                   MOVE      SYU-F05(1:4)  TO  TORIKOMI-YYYY
                   MOVE      SYU-F05(5:2)  TO  TORIKOMI-MM
                   MOVE      SYU-F05(7:2)  TO  TORIKOMI-DD
                   MOVE      SYU-F06(1:2)  TO  TORIKOMI-HH
                   MOVE      SYU-F06(3:2)  TO  TORIKOMI-MN
                   MOVE      WK-MST-TORIKOMI-BI
                                           TO  MSBI
                   MOVE      SYU-F07       TO  MSSU
         END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
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
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
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
     MOVE     JDATE               TO   PARA-DATE.
     MOVE     JTIME               TO   PARA-TIME.
     MOVE     TORICD              TO   PARA-TORICD.
     MOVE     NOUDTS              TO   PARA-NOBIS.
     MOVE     NOUDTE              TO   PARA-NOBIE.
     MOVE     KEPKBN              TO   PARA-KBN.
*出荷停止欠品の場合、納品日範囲を最大範囲をセット
     IF  KEPKBN = "2"
         MOVE ZERO                TO   PARA-NOBIS
         MOVE 99999999            TO   PARA-NOBIE
     END-IF.
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
     MOVE     "SCREEN"       TO   DSP-GRP.
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
     WRITE    FJK01001.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS
       INVALID
              MOVE      SPACE          TO   TOK-INVALID-FLG
              MOVE      SPACE          TO   TOK-F03
       NOT INVALID
              MOVE      "1"            TO   TOK-INVALID-FLG
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷制限マスタ　 READ                        *
*--------------------------------------------------------------*
 900-SYU-READ           SECTION.
     MOVE     "900-SYU-READ"      TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE      SPACE              TO   SYU-INVALID-FLG.
*
*----<< キー項目セット >>--*
     MOVE      TORICD             TO   SYU-F01.
     MOVE      ZERO               TO   SYU-F02.
     MOVE      SPACE              TO   SYU-F03.
*
*----<< FILE START >>--*
     START     SYUSGNF   KEY  >=   SYU-F01
                                   SYU-F02
                                   SYU-F03
       INVALID   KEY
             MOVE      SPACE           TO   SYU-INVALID-FLG
             GO   TO   900-SYU-READ-EXIT
       NOT INVALID
             CONTINUE
     END-START.
*
*----<< FILE READ >>--*
     READ     SYUSGNF
       AT END
              MOVE      SPACE          TO   SYU-INVALID-FLG
              GO   TO   900-SYU-READ-EXIT
       NOT AT END
              CONTINUE
     END-READ.
*
*----<< FILE CHECK >>--*
     IF  TORICD    =    SYU-F01
       THEN
              MOVE      "1"            TO   SYU-INVALID-FLG
       ELSE
              MOVE      SPACE          TO   SYU-INVALID-FLG
              GO   TO   900-SYU-READ-EXIT
     END-IF.
 900-SYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル  READ                       *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE      SPACE              TO   DEN-INVALID-FLG.
*
*----<< キー項目セット >>--*
     MOVE      JDATE              TO   DEN-F46.
     MOVE      JTIME              TO   DEN-F47.
     MOVE      TORICD             TO   DEN-F01.
*
*----<< FILE START >>--*
     START     SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                   DEN-F01   DEN-F48
                                   DEN-F02   DEN-F04
                                   DEN-F051  DEN-F07
                                   DEN-F112  DEN-F03
       INVALID   KEY
             MOVE      SPACE           TO   DEN-INVALID-FLG
             GO   TO   900-DEN-READ-EXIT
       NOT INVALID
             CONTINUE
     END-START.
*
*----<< FILE READ >>--*
     READ     SHTDENF
       AT END
              MOVE      SPACE          TO   DEN-INVALID-FLG
              GO   TO   900-DEN-READ-EXIT
       NOT AT END
              CONTINUE
     END-READ.
*
*----<< FILE CHECK >>--*
     IF ( JDATE     =    DEN-F46 ) AND
        ( JTIME     =    DEN-F47 ) AND
        ( TORICD    =    DEN-F01 )
       THEN
              MOVE      "1"            TO   DEN-INVALID-FLG
       ELSE
              MOVE      SPACE          TO   DEN-INVALID-FLG
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF NOUDTS
                                  EDIT-CURSOR OF NOUDTE
                                  EDIT-CURSOR OF KEPKBN.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF NOUDTS
                                  EDIT-OPTION OF NOUDTE
                                  EDIT-OPTION OF KEPKBN.
 CLR-HEAD-EXIT.
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