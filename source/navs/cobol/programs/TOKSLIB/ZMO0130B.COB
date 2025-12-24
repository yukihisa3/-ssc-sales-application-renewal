****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　売上データ未抽出チェックリスト　　*
*    作成日／更新日　　　：　93/07/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上データより，未抽出チェック　　*
*                        ：　リストを作成する。　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0130B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/07/02.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K6500.
 OBJECT-COMPUTER.       K6500.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-S-INFILE
                        ACCESS         MODE SEQUENTIAL
                        STATUS         DEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS         MODE RANDOM
                        RECORD         KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS         MODE RANDOM
                        RECORD         KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-PGID            PIC  X(08)  VALUE  "ZMO0130B".
 01  FLAGS.
     03  END-FLG        PIC  X(03).
 01  COUNTERS.
     03  IN-CNT         PIC  9(05).
     03  OUT-CNT        PIC  9(05).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(05).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-32          PIC  X(25).
     03  WK-33          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-34          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-35          PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-TOR                    PIC  9(08).
             07  NEW-DEN                    PIC  X(09).
             07  NEW-KUB                    PIC  9(01).
     03  OLD.
         05  OLD-01.
             07  OLD-TOR                    PIC  9(08).
             07  OLD-DEN                    PIC  X(09).
             07  OLD-KUB                    PIC  9(01).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  HD-PGID    PIC  X(08).
         05  FILLER     PIC  X(16)     VALUE  SPACE.
         05  FILLER     PIC  N(19)     VALUE
             NC"【　売上伝票　未抽出チェックリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"相".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(22)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文_".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"請区".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
         05  FILLER     PIC  X(42)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(03)     VALUE  NC"スト_".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(07).
         05  FILLER     PIC  X(01).
         05  ME-14      PIC  9(06).
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  9(06).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(04).
         05  FILLER     PIC  X(05).
         05  ME-17      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-19      PIC  9(01).
         05  FILLER     PIC  X(08).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-20      PIC  N(02).
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  X(30).
         05  ME-24      PIC  --,---,--9.99.
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(01).
         05  ME-26      PIC  --,---,--9.99.
         05  ME-27      PIC  ---,---,--9.
         05  ME-28      PIC  --,---,--9.99.
         05  ME-29      PIC  ---,---,--9.
         05  FILLER     PIC  X(01).
         05  ME-30      PIC  X(05).
         05  FILLER     PIC  X(01).
         05  ME-31      PIC  X(10).
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-32L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-32      PIC  X(25).
         05  ME-32R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-33      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  ME-34      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-35      PIC  ----,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL   "-".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0130B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
**
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HDENJNL.
     MOVE     "HDENJNL"      TO   ERR-FL-ID.
     MOVE     DEN-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE     "HTOKMS "      TO   ERR-FL-ID.
     MOVE     TOK-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTENMS.
     MOVE     "HTENMS "      TO   ERR-FL-ID.
     MOVE     TEN-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*               コントロール
****************************************************************
 CONTROL-SEC         SECTION.
     DISPLAY  "***   ZMO0130B START  ***"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    OLD  =  HIGH-VALUE.
     PERFORM  END-SEC.
*
     DISPLAY  "***   ZMO0130B  END   ***"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-SEC               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
*
     OPEN     INPUT     HDENJNL.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT.
*
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     PERFORM   DEN-READ.
     IF       NEW       =    HIGH-VALUE
              MOVE      HIGH-VALUE     TO   OLD
     END-IF.
*
 INIT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              NOT AT END
              ADD       1              TO   IN-CNT
     END-READ.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      DEN-F01        TO   NEW-TOR
              MOVE      DEN-F02        TO   NEW-DEN
              MOVE      DEN-F04        TO   NEW-KUB
     END-IF.
 DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-SEC               SECTION.
*
     IF     ( OLD  NOT  =    LOW-VALUE ) AND
            ( NEW  NOT  =    OLD )
              PERFORM   MEIS03-PRINT
     END-IF.
     IF     ( NEW  NOT  =    HIGH-VALUE )  AND
            ( NEW  NOT  =    OLD )
              PERFORM   MEIS01-PRINT
     END-IF.
     IF     ( NEW       NOT  =    HIGH-VALUE )  AND
            ( DEN-F03   NOT  =    80 )
              PERFORM   MEIS02-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
*
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   SYUKEI
              PERFORM   DEN-READ
     END-IF.
*
 MAIN-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE    HDENJNL.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     DISPLAY  "***   HDENJNL    IN  = "  IN-CNT  UPON  CONS.
     DISPLAY  "***   ﾍﾟｰｼﾞｽｳ   PAGE = " PAGE-CNT UPON  CONS.
 END-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEIS01-PRINT      SECTION.
     IF       LINE-CNT  >    62
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F04        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F01        TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   ME-05.
     MOVE     DEN-F02        TO   ME-06.
     MOVE     DEN-F08        TO   ME-07.
     MOVE     DEN-F09        TO   ME-08.
     MOVE     DEN-F07        TO   ME-09.
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F03        TO   ME-10.
     MOVE     DEN-F051       TO   ME-11.
     MOVE     DEN-F052       TO   ME-12.
     MOVE     DEN-F10        TO   ME-13.
     MOVE     DEN-F111       TO   ME-14.
     MOVE     DEN-F112       TO   ME-15.
     MOVE     DEN-F12        TO   ME-16.
     MOVE     DEN-F131       TO   ME-17.
     MOVE     DEN-F132       TO   ME-18.
     MOVE     DEN-F134       TO   ME-19.
     IF       DEN-F133  =    9
              MOVE      NC"請求"  TO   ME-20
     ELSE
              MOVE      SPACE     TO   ME-20
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     2.
     WRITE    PRT-REC        FROM P-SPACE   AFTER     1.
     ADD      3              TO   LINE-CNT.
 MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEIS02-PRINT      SECTION.
     IF       LINE-CNT  >    63
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     DEN-F03        TO   ME-21.
     IF       DEN-F25   NOT  =    SPACE
              MOVE      DEN-F25   TO   ME-22
     ELSE
              MOVE      DEN-F141  TO   ME-22
     END-IF.
     MOVE     DEN-F142       TO   ME-23.
     MOVE     DEN-F15        TO   ME-24.
     MOVE     DEN-F16        TO   ME-25.
     MOVE     DEN-F172       TO   ME-26.
     MOVE     DEN-F181       TO   ME-27.
     MOVE     DEN-F173       TO   ME-28.
     MOVE     DEN-F182       TO   ME-29.
     MOVE     DEN-F21        TO   ME-30.
     MOVE     DEN-F22        TO   ME-31.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
     ADD      1              TO   LINE-CNT.
 MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEIS03-PRINT      SECTION.
     IF       LINE-CNT  >    62
              PERFORM   HEAD-PRINT
     END-IF.
*
     IF       WK-32     NOT  =    SPACE
              MOVE      WK-32          TO   ME-32
              MOVE      NC"備考（"     TO   ME-32L
              MOVE      NC"）"         TO   ME-32R
     ELSE
              MOVE      SPACE          TO   ME-32
              MOVE      SPACE          TO   ME-32L
              MOVE      SPACE          TO   ME-32R
     END-IF.
     MOVE     WK-33          TO   ME-33.
     MOVE     WK-34          TO   ME-34.
     MOVE     WK-35          TO   ME-35.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM P-LINE    AFTER     1.
     ADD      3              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
 MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 SYUKEI            SECTION.
     IF       DEN-F03   =    80
              MOVE      DEN-F142       TO   WK-32
     END-IF.
     ADD      DEN-F15        TO   WK-33.
     ADD      DEN-F181       TO   WK-34.
     ADD      DEN-F182       TO   WK-35.
 SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 HEAD-PRINT        SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     WK-PGID             TO   HD-PGID.
     MOVE     LINK-OUT-YMD(1:4)   TO   HD-011.
     MOVE     LINK-OUT-YMD(5:2)   TO   HD-012.
     MOVE     LINK-OUT-YMD(7:2)   TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     MOVE     6              TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 TEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
