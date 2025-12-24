****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替入荷予定リスト出力        *
*    作成日／更新日　　　：　2017/01/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    流用元　　　　　　　：　SFU3120L                          *
*    処理概要　　　　　　：　社内振替データより、未完納分の　　*
*                        ：　入荷予定をリストアウトする。　    *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU3150L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/01/20.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 振替ワーク >>--*
     SELECT   SFRLSTW3  ASSIGN         DA-01-VI-SFRLSTW3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  LST-F04   LST-F12
                                       LST-F01   LST-F02
                                       LST-F03   LST-F14
                                       LST-F05   LST-F06
                                       LST-F07   LST-F08
                        STATUS         SFRLSTW3-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*----<< 仕入先マスタ >>--*
     SELECT   ZSHIMS1   ASSIGN         DA-01-VI-ZSHIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHI-F01
                        STATUS         ZSHIMS1-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 振替ワーク >>--*
 FD  SFRLSTW3           LABEL     RECORD   IS   STANDARD.
     COPY     SFRLSTW3  OF        XFDLIB
              JOINING   LST       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< 仕入先マスタ >>--*
 FD  ZSHIMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSHIMS1   OF        XFDLIB
              JOINING   SHI       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ZSOKMS1-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  ZSHIMS1-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  JYOKEN-INV-FLG  PIC  X(03)  VALUE  SPACE.
     03  END-FLG         PIC  X(03)  VALUE  SPACE.
*
 01  COUNTER.
     03  LINE-CNT        PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT        PIC  9(03)   VALUE  ZERO.
     03  READ-CNT        PIC  9(07)   VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SFRLSTW3-ST         PIC  X(02).
 01  ZSOKMS1-ST          PIC  X(02).
 01  ZSHIMS1-ST          PIC  X(02).
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
 01  ZAN                PIC S9(09).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-DENKBN                 PIC  X(02).
             07  NEW-NENDO                  PIC  9(04).
             07  NEW-SEASON                 PIC  X(02).
             07  NEW-SOKO                   PIC  X(02).
             07  NEW-SYOHIN                 PIC  X(08).
             07  NEW-HIN1                   PIC  X(05).
             07  NEW-HIN2                   PIC  X(02).
             07  NEW-HIN3                   PIC  X(01).
     03  OLD.
         05  OLD-01.
             07  OLD-DENKBN                 PIC  X(02).
             07  OLD-NENDO                  PIC  9(04).
             07  OLD-SEASON                 PIC  X(02).
             07  OLD-SOKO                   PIC  X(02).
             07  OLD-SYOHIN                 PIC  X(08).
             07  OLD-HIN1                   PIC  X(05).
             07  OLD-HIN2                   PIC  X(02).
             07  OLD-HIN3                   PIC  X(01).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SFU3150L".
         05  FILLER     PIC  X(24)     VALUE  SPACE.
         05  FILLER     PIC  N(13)     VALUE
             NC"＊社内振替入荷予定リスト＊".
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(26)     VALUE  SPACE.
         05  HD-011     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-014     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-015     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-016     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-017     PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
*
*01  HEAD02.
*    03  FILLER         CHARACTER TYPE PITCH-2.
*        05  FILLER     PIC  X(01)     VALUE  SPACE.
*        05  FILLER     PIC  N(05)     VALUE  NC"ＤＴ区分：".
*        05  HD-021     PIC  X(01).
*        05  FILLER     PIC  X(02)     VALUE  SPACE.
*        05  HD-022     PIC  N(05).
*        05  FILLER     PIC  X(04)     VALUE  SPACE.
*        05  FILLER     PIC  N(05)     VALUE  NC"完納区分：".
*        05  HD-023     PIC  X(01).
*        05  FILLER     PIC  X(01)     VALUE  SPACE.
*        05  HD-024     PIC  N(05).
*
 01  HEAD03.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"倉庫：".
         05  HD-031     PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-032     PIC  N(10).
*
 01  HEAD04.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"仕入先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"伝".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"年度".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"シ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"入荷予定日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"商品情報".
         05  FILLER     PIC  X(48)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"棚　番".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"入荷予定数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
*
*01  HEAD05.
*    03  FILLER         CHARACTER TYPE PITCH-2.
*        05  FILLER     PIC  X(33)     VALUE  SPACE.
*        05  FILLER     PIC  N(03)     VALUE  NC"ＭＳＧ".
*
 01  HEAD06.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"仕入先名".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01).
         05  ME-011     PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-012     PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-013     PIC  X(04).
         05  FILLER     PIC  X(01).
         05  ME-014     PIC  X(02).
         05  FILLER     PIC  X(01).
         05  ME-015     PIC  X(10).
         05  FILLER     PIC  X(01).
         05  ME-016     PIC  X(08).
         05  FILLER     PIC  X(01).
         05  ME-017     PIC  N(30).
         05  FILLER     PIC  X(02).
         05  ME-018     PIC  X(06).
         05  FILLER     PIC  X(01).
         05  ME-019     PIC  ---,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-01A     PIC  X(01).
         05  FILLER     PIC  X(25).
         05  ME-01B     PIC  X(01).
*
*01  MEIS02.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
*        05  FILLER     PIC  X(33).
*        05  ME-021     PIC  N(30).
*
 01  MEIS03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01).
         05  ME-031     PIC  N(16).
*
*01  MEIS04.
*    03  FILLER         CHARACTER TYPE PITCH-1-5.
*        05  FILLER     PIC  X(33)     VALUE  SPACE.
*        05  FILLER     PIC  N(02)     VALUE  NC"合計".
*        05  FILLER     PIC  X(01)     VALUE  SPACE.
*        05  ME-041     PIC  ---,---,--9.
*        05  FILLER     PIC  X(01)     VALUE  SPACE.
*        05  ME-042     PIC  ---,---,--9.
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "=".
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
 LINKAGE                SECTION.
 01  PARA-IN-OUTSOKO        PIC   X(02).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-IN-OUTSOKO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 振替ワーク >>--*
 SFRLSTW3-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SFRLSTW3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SFU3150L SFRLSTW3 ERROR " SFRLSTW3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SFU3150L ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 仕入先マスタ >>--*
 ZSHIMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSHIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SFU3150L ZSHIMS1 ERROR " ZSHIMS1-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*システム日付取得
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
*システム時刻取得
     ACCEPT   SYS-TIME       FROM TIME.
*システム時刻表示
     DISPLAY  "*** SFU3150L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SFRLSTW3.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     ZSHIMS1.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     99                  TO   LINE-CNT.
     MOVE     ZERO                TO   PAGE-CNT.
*    INITIALIZE                        BREAK-KEY  GOKEI-AREA.
*振替ワークスタート
     MOVE     SPACE               TO   LST-REC.
     INITIALIZE                        LST-REC.
     START  SFRLSTW3  KEY  IS  >=  LST-F04  LST-F12  LST-F01
                                   LST-F02  LST-F03  LST-F14
                                   LST-F05  LST-F06  LST-F07
                                   LST-F08
            INVALID
            DISPLAY NC"＃＃出力対象データなし！＃＃" UPON CONS
            CLOSE   SFRLSTW3  ZSOKMS1  ZSHIMS1  PRTF
            STOP  RUN
     END-START.
*
     PERFORM  SFRLSTW3-READ-SEC.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
     PERFORM  LIST-PRINT-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SFRLSTW3.
     CLOSE    ZSOKMS1.
     CLOSE    ZSHIMS1.
     CLOSE    PRTF.
*
     DISPLAY NC"読込件数" " = " READ-CNT  NC"件" UPON CONS.
     DISPLAY NC"出力頁数" " = " PAGE-CNT  NC"頁" UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SFU3150L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 LIST-PRINT-SEC         SECTION.
*
     MOVE    "LIST-PRINT-SEC"     TO   S-NAME.
*明細印刷
     PERFORM  MEISAI1-PRINT-SEC
     PERFORM  MEISAI2-PRINT-SEC.
*    PERFORM  SYUKEI-SEC.
*
     PERFORM  SFRLSTW3-READ-SEC.
*
 LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEISAI1-PRINT-SEC      SECTION.
*
     MOVE    "MEISAI1-PRINT-SEC"  TO   S-NAME.
*
     IF       LINE-CNT >      50
              PERFORM   HEAD-PRINT-SEC
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*　仕入先ＣＤ
     MOVE     LST-F12        TO   ME-011.
*  伝票区分
     MOVE     LST-F01        TO   ME-012.
*  年度
     MOVE     LST-F02        TO   ME-013.
*  シーズン
     MOVE     LST-F03        TO   ME-014.
*  入荷予定日
     MOVE     LST-F14(1:4)   TO   ME-015(1:4).
     MOVE     "/"            TO   ME-015(5:1).
     MOVE     LST-F14(5:2)   TO   ME-015(6:2).
     MOVE     "/"            TO   ME-015(8:1).
     MOVE     LST-F14(7:2)   TO   ME-015(9:2).
*  商品情報
     MOVE     LST-F05        TO   ME-016.
*　商品名称
     MOVE     LST-F09        TO   ME-017(1:15).
     MOVE     LST-F10        TO   ME-017(16:15).
*　棚番
     MOVE     LST-F11        TO   ME-018.
*　入荷予定数
     COMPUTE  ZAN  =   LST-F15 - LST-F19.
     MOVE     ZAN            TO   ME-019.
*　備考
     MOVE     "("            TO   ME-01A.
     MOVE     ")"            TO   ME-01B.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
     MOVE     SPACE          TO   PRT-REC.
*
     ADD      1              TO   LINE-CNT.
*
 MEISAI1-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEISAI2-PRINT-SEC      SECTION.
*
     MOVE    "MEISAI2-PRINT-SEC"  TO   S-NAME.
*
     IF       LINE-CNT  >    50
              PERFORM   HEAD-PRINT-SEC
     END-IF.
*
     MOVE     SPACE          TO   MEIS03.
*　仕入先名
     MOVE     LST-F12        TO   SHI-F01.
     PERFORM  900-SHI-READ.
     MOVE     SHI-F02        TO   ME-031.
*
     WRITE    PRT-REC        FROM MEIS03    AFTER     1.
     WRITE    PRT-REC        FROM P-LINE    AFTER     1.
*
     ADD      2              TO   LINE-CNT.
*
 MEISAI2-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 HEAD-PRINT-SEC         SECTION.
*
     MOVE    "HEAD-PRINT-SEC"     TO   S-NAME.
*改頁判定
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     SYS-HH              TO   HD-014.
     MOVE     SYS-MN              TO   HD-015.
     MOVE     SYS-SS              TO   HD-016.
     MOVE     PAGE-CNT            TO   HD-017.
*出力倉庫
     MOVE     PARA-IN-OUTSOKO     TO   SOK-F01  HD-031.
     PERFORM  900-SOK-READ.
     MOVE     SOK-F02             TO   HD-032.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
*--- WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     3.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
*--- WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD06    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
*
     MOVE     10             TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    振分ワーク　　 READ                          *
*--------------------------------------------------------------*
 SFRLSTW3-READ-SEC       SECTION.
*
     MOVE     "SFRLSTW3-READ-SEC"  TO   S-NAME.
*
     READ     SFRLSTW3   AT   END
              MOVE      "END"     TO   END-FLG
              GO   TO    SFRLSTW3-READ-EXIT
     END-READ.
*
     ADD      1                   TO   READ-CNT.
*
     IF  READ-CNT(5:3) = "500" OR "000"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
     MOVE     NEW                 TO   OLD.
*
 SFRLSTW3-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    倉庫マスタ　 READ                          *
*--------------------------------------------------------------*
 900-SOK-READ           SECTION.
     MOVE     "900-SOK-READ"      TO   S-NAME.
     READ     ZSOKMS1   INVALID
              MOVE      SPACE          TO   SOK-F03
     END-READ.
 900-SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    仕入先マスタ　READ                           *
*--------------------------------------------------------------*
 900-SHI-READ           SECTION.
     MOVE     "900-SHI-READ"      TO   S-NAME.
     READ     ZSHIMS1    INVALID
              MOVE      SPACE          TO   SHI-F03
     END-READ.
 900-SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
