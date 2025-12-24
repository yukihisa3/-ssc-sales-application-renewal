****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　量販店チェックリスト　　　　　　　*
*    作成日／作成者　　　：　99/10/06  HAGIWARA                *
*    　　　　　　　　　　：　画面日付，時刻表示　　　　　　　　*
*                        ：　日付変換サブルーチン使用　　　　　*
*    再利用ＰＧ　　　　　：  SSKT040.SKTSLIB                   *
*    更新日／更新者　　　：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY0602L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/10/06.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         21-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 量販データ >>--*
     SELECT   SHTRYTF   ASSIGN         DA-01-VI-SHTRYTL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RYO-F011  RYO-F012  RYO-F02
                        STATUS         SHTRYTF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 商品変換テーブル >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01   SHO-F02
                        STATUS         HSHOTBL-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY06021  OF        XMDLIB.
*----<< 量販データ >>--*
 FD  SHTRYTF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTRYTF   OF        XFDLIB
              JOINING   RYO       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01).
     03  SEN-FLG        PIC  9(01)     VALUE ZERO.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
     03  IX             PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTRYTF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HSHOTBL-ST        PIC  X(02).
*
*----<< ﾌﾟﾘﾝﾄ ﾜｰｸ >>--*
 01  WK-032.
     03  WK-0321        PIC  X(05).
     03  WK-0322        PIC  X(02).
     03  WK-0323        PIC  X(01).
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-SUU        PIC  S9(11)    PACKED-DECIMAL.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE.
     03  WK-YS          PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y       PIC  9(02)  VALUE ZERO.
         05  WK-M       PIC  9(02)  VALUE ZERO.
         05  WK-D       PIC  9(02)  VALUE ZERO.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*----<< ﾊﾝｲ ﾜｰｸ >>--*
 01  ST-MEMO-X.
     03  ST-MEMO        PIC  9(04).
     03  ST-EDA         PIC  9(02).
 01  ED-MEMO-X.
     03  ED-MEMO        PIC  9(04).
     03  ED-EDA         PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-MEMO-1                 PIC  9(04).
             07  NEW-MEMO-2                 PIC  9(02).
     03  OLD.
         05  OLD-01.
             07  OLD-MEMO-1                 PIC  9(04).
             07  OLD-MEMO-2                 PIC  9(02).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
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
 01  GUIDE01       PIC  N(30)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(30)  VALUE
         NC"_取　消　_終　了　_再入力".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(45)     VALUE  SPACE.
         05  FILLER     PIC  N(16)     VALUE
                        NC"【　量販店入力チェックリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
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
         05  FILLER     PIC  N(05)     VALUE  NC"取　引　先".
         05  FILLER     PIC  X(16)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"メモ_".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文_".
         05  FILLER     PIC  X(11)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(36)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"品".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"原単価".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"売単価".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"摘　要".
         05  FILLER     PIC  X(26)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(10).
         05  FILLER     PIC  X(03).
         05  ME-05      PIC  ZZZ9.
         05  FILLER     PIC  X(03).
         05  ME-06      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(05).
         05  ME-08      PIC  X(15).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(06).
         05  FILLER     PIC  X(03).
         05  ME-10      PIC  9(06).
         05  FILLER     PIC  X(02).
         05  ME-11      PIC  X(04).
         05  FILLER     PIC  X(06).
         05  ME-12      PIC  X(02).
         05  FILLER     PIC  X(04).
         05  ME-13      PIC  X(02).
         05  FILLER     PIC  X(08).
         05  ME-14      PIC  9(01).
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(15).
         05  FILLER     PIC  X(01).
         05  ME-17      PIC  X(15).
         05  FILLER     PIC  X(01).
         05  ME-18      PIC  X(08).
         05  FILLER     PIC  X(01).
         05  ME-19      PIC  X(05).
         05  FILLER     PIC  X(01).
         05  ME-20      PIC  X(02).
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  X(01).
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  ZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  ZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01).
         05  ME-24      PIC  X(15).
         05  FILLER     PIC  X(01).
         05  ME-25      PIC  X(15).
         05  FILLER     PIC  X(01).
         05  ME-26      PIC  X(10).
 01  MEIS03.
         05  ME-27X          OCCURS    17.
             07  FILLER      PIC  X(01).
             07  ME-27L      PIC  X(01).
             07  ME-27       PIC  ZZZZ9.
             07  ME-27R      PIC  X(01).
 01  MEIS04.
     03  FILLER              OCCURS    17.
         05  ME-28X.
             07  FILLER      PIC  X(01).
             07  ME-28       PIC  ZZ,ZZ9.
             07  FILLER      PIC  X(01).
         05  FILLER          REDEFINES      ME-28X.
             07  ME-281      PIC  9(04).
             07  FILLER      PIC  X(01).
             07  ME-282      PIC  9(02).
 01  MEIS05             CHARACTER TYPE MODE-2.
     03  J-LINE-X       PIC  N(068)    VALUE  ALL  NC"─".
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL  "-".
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0602L DSPFILE ERROR " DSP-CNTL " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTRYTF   HTOKMS    HSHOTBL   DSPFILE.
     STOP     RUN.
*----<< 量販データ >>--*
 SHTRYTF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTRYTF.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0602L SHTRYTF ERROR " SHTRYTF-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTRYTF   HTOKMS    HSHOTBL   DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0602L HTOKMS ERROR " HTOKMS-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTRYTF   HTOKMS    HSHOTBL   DSPFILE.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0602L HSHOTBL ERROR " HSHOTBL-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTRYTF   HTOKMS    HSHOTBL   DSPFILE.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
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
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時刻取得
     ACCEPT    SYS-TIME         FROM   TIME.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*
     DISPLAY  "*** SSY0602L START *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" WK-M ":" SYS-SS UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       SHTRYTF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HSHOTBL.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｲﾝｻﾂ >>-*
     PERFORM  240-PRINT      UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSPFILE.
     CLOSE    SHTRYTF.
     CLOSE    HTOKMS.
     CLOSE    HSHOTBL.
*
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0602L END *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FSY06021.
*システム日付転送
     MOVE     HEN-DATE       TO   SDATE.
*システム時間転送
     MOVE    HEN-TIME        TO   STIME.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY06021"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "R00001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              EVALUATE  R00001
                   WHEN 1
                   WHEN 2
                        MOVE      9         TO   GR-NO
                        MOVE      SPACE     TO   S00002
                        MOVE      SPACE     TO   S00003
                        MOVE      SPACE     TO   S00004
                        MOVE      SPACE     TO   S00005
                        MOVE      ZERO      TO   ST-MEMO
                        MOVE      ZERO      TO   ST-EDA
                        MOVE      99999     TO   ED-MEMO
                        MOVE      99        TO   ED-EDA
                   WHEN 3
                        MOVE      2         TO   GR-NO
                   WHEN OTHER
                        MOVE NC"指定コードが違います" TO   MSG
              END-EVALUATE
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "GRP01"        TO   WK-GRP.
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
              MOVE      ZERO      TO   ST-MEMO
              MOVE      ZERO      TO   ST-EDA
              MOVE      99999     TO   ED-MEMO
              MOVE      99        TO   ED-EDA
              IF        R00002    IS   NUMERIC
                        MOVE      R00002    TO   ST-MEMO
              END-IF
              IF        R00003    IS   NUMERIC
                        MOVE      R00003    TO   ST-EDA
              END-IF
              IF        R00004    IS   NUMERIC
                        MOVE      R00004    TO   ED-MEMO
              END-IF
              IF        R00005    IS   NUMERIC
                        MOVE      R00005    TO   ED-EDA
              END-IF
              IF        ST-MEMO-X >    ED-MEMO-X
                        MOVE NC"範囲指定が違います"   TO   MSG
              ELSE
                        MOVE      9         TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "KKNN"         TO   WK-GRP.
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
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE              GOKEI-AREA.
     MOVE     0              TO   IX.
     MOVE     SPACE          TO   MEIS03.
     MOVE     SPACE          TO   MEIS04.
*
     MOVE     ST-MEMO-X      TO   RYO-F01.
     MOVE     ZERO           TO   RYO-F02.
     PERFORM  900-RYO-START-READ.
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     MOVE     0              TO   GR-NO.
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     IF       OLD  NOT  =    LOW-VALUE
     AND      NEW  NOT  =    OLD
              ADD       1    TO   IX
              MOVE      " *ｺﾞｳｹｲ*"     TO   ME-27X(IX)
              MOVE      KEI-SUU        TO   ME-28 (IX)
              INITIALIZE          GOKEI-AREA
              MOVE      1  TO   SEN-FLG
              PERFORM   2412-MEIS03-PRINT
     END-IF.
*
     IF       NEW  NOT       =    HIGH-VALUE
     AND      NEW  NOT       =    OLD
              PERFORM   2411-MEIS01-SET
              PERFORM   2411-MEIS01-PRINT
              ADD       1              TO   IX
              MOVE      "ﾒﾓ NO."       TO   ME-27X(IX)
              MOVE      RYO-F011       TO   ME-281(IX)
              MOVE      RYO-F012       TO   ME-282(IX)
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   2413-MEIS-SET
              PERFORM   900-RYO-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-SET        SECTION.
*転送（ＭＥＩＳ０１）
     MOVE     SPACE          TO   MEIS01.
     MOVE     RYO-F11        TO   ME-03.
     MOVE     RYO-F11        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     RYO-F011       TO   ME-05.
     MOVE     RYO-F04        TO   ME-06.
     MOVE     RYO-F05        TO   ME-07.
     MOVE     RYO-F63        TO   ME-08.
     MOVE     RYO-F071       TO   ME-09.
     MOVE     RYO-F072       TO   ME-10.
     MOVE     RYO-F08        TO   ME-11.
     MOVE     RYO-F09        TO   ME-12.
     MOVE     RYO-F10        TO   ME-13.
*\\  93.06.30 START  \\
*****MOVE     RYO-F572       TO   ME-14.
     MOVE     RYO-F60        TO   ME-14.
*\\  93.06.30 END    \\
*転送（ＭＥＩＳ０２）
     MOVE     SPACE          TO   MEIS02.
     MOVE     RYO-F121       TO   ME-15.
     MOVE     RYO-F1211      TO   ME-16.
     MOVE     RYO-F1212      TO   ME-17.
**   MOVE     RYO-F11        TO   SHO-F01.
**   MOVE     RYO-F121       TO   SHO-F02.
**   PERFORM  900-SHO-READ.
**   MOVE     SHO-F031       TO   ME-18.
**   MOVE     SHO-F032       TO   WK-032.
     MOVE     RYO-F621       TO   ME-18.
     MOVE     RYO-F622       TO   WK-032.
     MOVE     WK-0321        TO   ME-19.
     MOVE     WK-0322        TO   ME-20.
     MOVE     WK-0323        TO   ME-21.
     MOVE     RYO-F131       TO   ME-22.
     MOVE     RYO-F132       TO   ME-23.
     MOVE     RYO-F141       TO   ME-24.
     MOVE     RYO-F142       TO   ME-25.
     MOVE     RYO-F15        TO   ME-26.
 2411-MEIS01-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
*改頁
     IF       LINE-CNT  >    58
       PERFORM   2414-HEAD-PRINT
     ELSE
*印刷
       WRITE    PRT-REC        FROM MEIS01    AFTER     1
       WRITE    PRT-REC        FROM MEIS02    AFTER     1
       ADD      2              TO   LINE-CNT
     END-IF.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS03-PRINT      SECTION.
     IF       LINE-CNT  >    60
              PERFORM   2414-HEAD-PRINT
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS03    AFTER     1.
     WRITE    PRT-REC        FROM MEIS04    AFTER     1.
     IF    SEN-FLG   =   1
       WRITE    PRT-REC      FROM MEIS05    AFTER     1
       MOVE     ZERO     TO  SEN-FLG
     ELSE
       WRITE    PRT-REC      FROM P-LINE    AFTER     1
     END-IF.
     ADD      3              TO   LINE-CNT.
     MOVE     SPACE          TO   MEIS03.
     MOVE     SPACE          TO   MEIS04.
     MOVE     0              TO   IX.
 2412-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｾｯﾄ                                     *
*--------------------------------------------------------------*
 2413-MEIS-SET          SECTION.
     PERFORM   24131-TBL-SET VARYING   I    FROM 1    BY   1
                             UNTIL     I    >    50.
 2413-MEIS-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｾｯﾄ                                     *
*--------------------------------------------------------------*
 24131-TBL-SET          SECTION.
     IF       RYO-F162 (I)   NOT  =    ZERO
              ADD       1    TO   IX
              MOVE      "("            TO   ME-27L(IX)
              MOVE      RYO-F161 (I)   TO   ME-27 (IX)
              MOVE      ")"            TO   ME-27R(IX)
              MOVE      RYO-F162 (I)   TO   ME-28 (IX)
              ADD       RYO-F162 (I)   TO   KEI-SUU
     END-IF.
     IF       IX   =    17
              PERFORM   2412-MEIS03-PRINT
     END-IF.
 24131-TBL-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2414-HEAD-PRINT        SECTION.
     IF       PAGE-CNT  =    0
              PERFORM   2411-MEIS01-SET
     ELSE
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-DATE(1:4)       TO   HD-011.
     MOVE     SYS-DATE(5:2)       TO   HD-012.
     MOVE     SYS-DATE(7:2)       TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     8              TO   LINE-CNT.
*
     PERFORM   2411-MEIS01-PRINT.
 2414-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "SCRERE"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
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
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY06021.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    量販データ　 START READ                    *
*--------------------------------------------------------------*
 900-RYO-START-READ     SECTION.
     START    SHTRYTF   KEY  >=   RYO-F011  RYO-F012  RYO-F02
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-RYO-READ
     END-IF.
 900-RYO-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    量販データ　 READ                          *
*--------------------------------------------------------------*
 900-RYO-READ           SECTION.
     READ     SHTRYTF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-RYO-READ-EXIT
     END-READ.
     IF       RYO-F01   >    ED-MEMO-X
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
*
     MOVE     0         TO   SKIP-FLG.
     IF       R00001         =    1
     AND      RYO-F571  NOT  =    0
              MOVE      1         TO   SKIP-FLG
     END-IF.
     IF       R00001         =    2
     AND      RYO-F573  NOT  =    9
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
     AND      SKIP-FLG       =    0
              MOVE      9    TO   RYO-F571
     END-IF.
     REWRITE  RYO-REC.
     IF       NEW       NOT  =    HIGH-VALUE
     AND      SKIP-FLG       =    1
              GO   TO   900-RYO-READ
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      RYO-F011       TO   NEW-MEMO-1
              MOVE      RYO-F012       TO   NEW-MEMO-2
     END-IF.
 900-RYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル　　READ                     *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
     READ     HSHOTBL    INVALID
              INITIALIZE          SHO-REC
     END-READ.
 900-SHO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
