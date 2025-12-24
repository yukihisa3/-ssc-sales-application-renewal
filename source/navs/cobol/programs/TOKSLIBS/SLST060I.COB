****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理                          *
*    業務名　　　　　　　：　マスタリスト                      *
*    モジュール名　　　　：　商品変換テーブルリスト発行        *
*    作成日／更新日　　　：　08/08/06                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、商品変換テーブル履歴    *
*                            発行を行なう条件を入力する。　    *
*                            存在チェックの実施                *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SLST060I.
 AUTHOR.                NAV.
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
*----<< 商品変換テーブル履歴ファイル >>--*
     SELECT   HSHOTBR   ASSIGN         DA-01-VI-HSHOTBR3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SHO-F03   SHO-F01
                                       SHO-F04
                        STATUS         SHO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FLST0601  OF        XMDLIB.
*----<< 商品変換テーブル履歴ファイル >>--*
 FD  HSHOTBR            LABEL     RECORD   IS   STANDARD.
     COPY     HSHOTBR   OF        XFDLIB
              JOINING   SHO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHO-ST            PIC  X(02).
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
              NC"日付論理エラー。".
     03  MSG04               PIC  N(20)  VALUE
              NC"この条件の対象データはありません。".
     03  MSG05               PIC  N(20)  VALUE
              NC"モードエラー".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       5.
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
 01  PARA-SYUBETU           PIC   X(01).
 01  PARA-TANST             PIC   X(02).
 01  PARA-TANED             PIC   X(02).
 01  PARA-NYUST             PIC   9(08).
 01  PARA-NYUED             PIC   9(08).
 01  PARA-SYUBETSU          PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-SYUBETU
                                       PARA-TANST
                                       PARA-TANED
                                       PARA-NYUST
                                       PARA-NYUED
                                       PARA-SYUBETSU.
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
     DISPLAY  "### SLST060I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DSPFILE  HSHOTBR.
     STOP     RUN.
*----<< 伝票データ >>--*
 HSHOTBR-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBR.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SLST060I HSHOTBR ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DSPFILE  HSHOTBR.
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
     DISPLAY  "*** SLST060I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HSHOTBR.
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
*----<< 範囲    ﾆｭｳﾘｮｸ >>-*
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
     CLOSE    HSHOTBR.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SLST060I END   *** "
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
     MOVE     SPACE          TO   FLST0601.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SLST060I"      TO   R001.
     MOVE    "FLST0601"      TO   R002.
*
     MOVE     SPACE          TO   TANST  TANED  PMODE.
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FLST0601"     TO   DSP-FMT.
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
*担当者ＣＤ判定
     IF       TANED  =  SPACE
              MOVE  "99"     TO   TANED
     END-IF.
*
     IF       TANST  >  TANED
              IF   ERR-FLG   =    ZERO
                   MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF TANST
              MOVE     "R"   TO   EDIT-OPTION OF TANST
              MOVE     "R"   TO   EDIT-OPTION OF TANED
     END-IF.
*入力日
     IF       NYUST  NOT  NUMERIC
              MOVE     ZERO       TO   NYUST
     END-IF.
*
     IF       NYUED  NOT  NUMERIC
              MOVE     99999999   TO   NYUED
     END-IF.
*
     IF       NYUST  >  NYUED
              IF   ERR-FLG   =    ZERO
                   MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF NYUST
              MOVE     "R"   TO   EDIT-OPTION OF NYUST
              MOVE     "R"   TO   EDIT-OPTION OF NYUED
     END-IF.
*モードチェック
     IF  PMODE  =  SPACE  OR  "1"  OR  "2"  OR  "3"
         CONTINUE
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      5    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF PMODE
         MOVE     "R"   TO   EDIT-OPTION OF PMODE
     END-IF.
*****売上伝票データ存在チェック
*    IF  ERR-FLG        =    ZERO
*        MOVE      SPACE          TO   SHO-REC
*        INITIALIZE                    SHO-REC
*        MOVE      TANST          TO   SHO-F03
*        MOVE      NYUST          TO   SHO-F01
*        MOVE      PMODE          TO   SHO-F04
*        START     HSHOTBR   KEY  >=   SHO-F03   SHO-F01
*                                      SHO-F04
*         INVALID   KEY
*         MOVE      4    TO   ERR-FLG
*         MOVE     "C"   TO   EDIT-CURSOR OF TANST
*         MOVE     "R"   TO   EDIT-OPTION OF TANST
*         MOVE     "R"   TO   EDIT-OPTION OF TANED
*         MOVE     "R"   TO   EDIT-OPTION OF NYUST
*         MOVE     "R"   TO   EDIT-OPTION OF NYUED
*         MOVE     "R"   TO   EDIT-OPTION OF PMODE
*         GO   TO   220-GRP01-CHECK-EXIT
*         NOT INVALID
*         READ      HSHOTBR
*             AT END
*             MOVE      4    TO   ERR-FLG
*             DISPLAY "AAA" UPON CONS
*             MOVE     "C"   TO   EDIT-CURSOR OF TANST
*             MOVE     "R"   TO   EDIT-OPTION OF TANED
*             MOVE     "R"   TO   EDIT-OPTION OF NYUST
*             MOVE     "R"   TO   EDIT-OPTION OF NYUED
*             MOVE     "R"   TO   EDIT-OPTION OF PMODE
*             GO   TO   220-GRP01-CHECK-EXIT
*             NOT AT END
*     DISPLAY "TANTOU = " TANST ":" TANED ":" SHO-F03  UPON CONS
*     DISPLAY "NYURYO = " NYUST ":" NYUED ":" SHO-F01  UPON CONS
*             IF   (( TANST <= SHO-F03 ) AND ( TANED >= SHO-F03 ))
*             AND  (( NYUST <= SHO-F01 ) AND ( NYUED >= SHO-F01 ))
*                  IF   PMODE  NOT =  SPACE
*                       IF   PMODE NOT = SHO-F04
*                            MOVE  4   TO   ERR-FLG
*             DISPLAY "BBB" UPON CONS
*                            MOVE "C"  TO   EDIT-CURSOR OF TANST
*                            MOVE "R"  TO   EDIT-OPTION OF TANST
*                                           EDIT-OPTION OF TANED
*                                           EDIT-OPTION OF NYUST
*                                           EDIT-OPTION OF NYUED
*                                           EDIT-OPTION OF PMODE
*                       END-IF
*                  END-IF
*             ELSE
*                  MOVE      4    TO   ERR-FLG
*             DISPLAY "CCC" UPON CONS
*                  MOVE "C"  TO   EDIT-CURSOR OF TANST
*                  MOVE "R"  TO   EDIT-OPTION OF TANST
*                  MOVE "R"  TO   EDIT-OPTION OF TANED
*                  MOVE "R"  TO   EDIT-OPTION OF NYUST
*                  MOVE "R"  TO   EDIT-OPTION OF NYUED
*                  MOVE "R"  TO   EDIT-OPTION OF PMODE
*             END-IF
*         END-READ
*        END-START
*****END-IF.
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
     MOVE     "1"                 TO   PARA-SYUBETU
     MOVE     TANST               TO   PARA-TANST
     MOVE     TANED               TO   PARA-TANED
     MOVE     NYUST               TO   PARA-NYUST
     MOVE     NYUED               TO   PARA-NYUED
     MOVE     PMODE               TO   PARA-SYUBETSU
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
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
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
     WRITE    FLST0601.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF TANST
                                  EDIT-CURSOR OF TANED
                                  EDIT-CURSOR OF NYUST
                                  EDIT-CURSOR OF NYUED
                                  EDIT-CURSOR OF PMODE.
     MOVE     "M"            TO   EDIT-OPTION OF TANST
                                  EDIT-OPTION OF TANED
                                  EDIT-OPTION OF NYUST
                                  EDIT-OPTION OF NYUED
                                  EDIT-OPTION OF PMODE.
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