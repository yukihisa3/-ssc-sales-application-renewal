# SSY5780I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5780I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　グッデイＥＤＩ　　　　　　        *
*    業務名　　　　　　　：　受注・売上　　　　　　　　        *
*    モジュール名　　　　：　不照合リスト発行指示　            *
*    作成日／更新日　　　：　2014/07/07                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　画面より、不照合リスト発行を行な　*
*                            う受信日の範囲を入力する。　　    *
*    作成日／更新日　　　：　2017/08/02                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　不照合リスト発行対象データが存在　*
*                            しない場合の対応実施　　　　　    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY5780I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/13.
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
*----<< グッデイ受領累積ファイル >>--*
     SELECT   GDJYURL3  ASSIGN        DA-01-VI-GDJYURL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JYR-F01   JYR-F02
                                       JYR-F03   JYR-F05
                                       JYR-F04   JYR-F07
                                       JYR-F08
                        STATUS         GDJYURL3-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY57801  OF        XMDLIB.
*----<< グッデイ受領累積ファイル >>--*
 FD  GDJYURL3           LABEL     RECORD   IS   STANDARD.
     COPY     GDJYURL3  OF        XFDLIB
              JOINING   JYR       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  GDJYURL3-ST       PIC  X(02).
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
              NC"開始が終了を超えています。".
     03  MSG03               PIC  N(20)  VALUE
              NC"対象データが存在しません。".
     03  MSG04               PIC  N(20)  VALUE
              NC"不照合リスト出力対象のデータがありません".
     03  MSG05               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG06               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG07               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG08               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG09               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG10               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG11               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
     03  MSG12               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　　　　　　　　　".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      12.
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
 01  PARA-KENDTS            PIC   9(08).
 01  PARA-KENDTE            PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-KENDTS
                                       PARA-KENDTE.
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
     DISPLAY  "### SSY5780I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*    CLOSE    GDJYURL3  DSPFILE.
     STOP     RUN.
*----<< グッデイ受領累積ファイル >>--*
 GDJYURL3-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      GDJYURL3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY5780I GDJYURL3 ERROR " GDJYURL3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*    CLOSE    GDJYURL3  DSPFILE.
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
     DISPLAY  "*** SSY5780I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     GDJYURL3.
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
*----<< ﾊﾝｲ       ﾆｭｳﾘｮｸ >>-*
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
     CLOSE    GDJYURL3.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY5780I END *** "
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
     MOVE     SPACE          TO   FSY57801.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY5780I"      TO   PGID.
     MOVE    "FSY57801"      TO   FORM.
*
     MOVE    SYS-DATEW       TO   KENDTS  KENDTE.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY57801"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲｼﾃｲ   ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
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
*    受信日付チェック
     IF  KENDTS    >    KENDTE
         IF  ERR-FLG   =    ZERO
             MOVE  2         TO   ERR-FLG
         END-IF
         MOVE     "C"        TO   EDIT-CURSOR OF KENDTS
         MOVE     "R"        TO   EDIT-OPTION OF KENDTS
         MOVE     "R"        TO   EDIT-OPTION OF KENDTE
         GO                  TO   220-GRP01-CHECK-EXIT
     END-IF.
*    基本情報ファイル存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   JYR-REC
         INITIALIZE                    JYR-REC
         MOVE      KENDTS         TO   JYR-F01
         START     GDJYURL3   KEY  >=  JYR-F01   JYR-F02
                                       JYR-F03   JYR-F05
                                       JYR-F04   JYR-F07
                                       JYR-F08
              INVALID   KEY
                   MOVE      3    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF KENDTS
                   MOVE     "R"   TO   EDIT-OPTION OF KENDTS
                   MOVE     "R"   TO   EDIT-OPTION OF KENDTE
                   GO   TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                   READ      GDJYURL3
                     AT END
                        MOVE      3    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF KENDTS
                        MOVE     "R"   TO   EDIT-OPTION OF KENDTS
                        MOVE     "R"   TO   EDIT-OPTION OF KENDTE
                        GO   TO   220-GRP01-CHECK-EXIT
                     NOT AT END
                        PERFORM   220-DATA-CHECK-SEC
                   END-READ
         END-START
     END-IF.
*    チェックＯＫ時
     IF      ERR-FLG  NOT =  ZERO
             IF   ERR-FLG   =    ZERO
                  MOVE      3    TO   ERR-FLG
             END-IF
             MOVE "C"  TO   EDIT-CURSOR OF KENDTS
             MOVE "R"  TO   EDIT-OPTION OF KENDTS
                            EDIT-OPTION OF KENDTE
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      対象データ存在チェック  2014.06.17          *
*--------------------------------------------------------------*
 220-DATA-CHECK-SEC    SECTION.
*
     MOVE     "220-DATA-CHECK-SEC "    TO   S-NAME.
*
*指定範囲チェック
     IF ( KENDTS    <=   JYR-F01 ) AND
        ( KENDTE    >=   JYR-F01 )
        CONTINUE
     ELSE
          MOVE      3    TO   ERR-FLG
          MOVE     "C"   TO   EDIT-CURSOR OF KENDTS
          MOVE     "R"   TO   EDIT-OPTION OF KENDTS
          MOVE     "R"   TO   EDIT-OPTION OF KENDTE
          GO   TO   220-DATA-CHECK-EXIT
     END-IF.
*ＵＭＣ不照合理由
     IF   JYR-F99  =  SPACE
          CONTINUE
     ELSE
          GO   TO   220-DATA-CHECK-EXIT
     END-IF.
*次伝票チェック
     READ GDJYURL3
       AT END
*#2017/08/02 NAV ST
**********MOVE  3   TO   ERR-FLG
          MOVE  4   TO   ERR-FLG
*#2017/08/02 NAV ED
          MOVE "C"  TO
          EDIT-CURSOR OF KENDTS
          MOVE "R"  TO
          EDIT-OPTION OF KENDTS
          EDIT-OPTION OF KENDTE
*#2017/08/02 NAV ST
          GO   TO   220-DATA-CHECK-EXIT
*#2017/08/02 NAV ED
     END-READ.
*
     GO   TO   220-DATA-CHECK-SEC.
*
 220-DATA-CHECK-EXIT.
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
     MOVE     KENDTS              TO   PARA-KENDTS.
     MOVE     KENDTE              TO   PARA-KENDTE.
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
     WRITE    FSY57801.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KENDTS
                                  EDIT-CURSOR OF KENDTE.
     MOVE     "M"            TO   EDIT-OPTION OF KENDTS
                                  EDIT-OPTION OF KENDTE.
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

```
