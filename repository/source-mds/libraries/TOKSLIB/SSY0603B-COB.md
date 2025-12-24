# SSY0603B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0603B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　量販店データ変換　　　　　　　　　*
*    作成日／作成者　　　：　99/10/06  HAGIWARA                *
*    再利用ＰＧ　　　　　：  SSKT050.SKTSLIB                   *
*    更新日／更新者　　　：　                                  *
*　　　　　　　　　　　　　　00/04/18  Y.YOSHIDA               *
*                          摘要を量販店備考ワークへ出力に変更  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY0603B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*量販データＦ
     SELECT      SHTRYTF     ASSIGN    TO        DA-01-S-SHTRYTF
                             FILE      STATUS    RYO-ST.
*伝票データＦ
     SELECT      SHTDENWK    ASSIGN    TO        DA-01-S-SHTDENWK
                             FILE      STATUS    DEN2-ST.
*量販店備考ワーク
     SELECT      SHTBIKF     ASSIGN         DA-01-VI-SHTBIKL1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE RANDOM
                                       RECORD    KEY  BIK-F01
                                                      BIK-F02
                                                      BIK-F03
                                       STATUS    TOK-ST.
*----<< 取引先マスタ >>--*
     SELECT      HTOKMS      ASSIGN              DA-01-VI-TOKMS2
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE RANDOM
                                       RECORD    KEY  TOK-F01
                                       STATUS    TOK-ST.
*----<< 店舗マスタ >>-*
     SELECT      HTENMS     ASSIGN               DA-01-VI-TENMS1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE DYNAMIC
                                       RECORD    KEY  TEN-F52
                                                      TEN-F011
                                       STATUS    TEN-ST.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN                   DA-01-VI-MEIMS1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE DYNAMIC
                                       RECORD    KEY  MEI-F01
                                       STATUS    MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*量販データＦ
 FD  SHTRYTF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        SHTRYTF     OF        XFDLIB
     JOINING     RYO         AS        PREFIX.
*伝票データＦ
 FD  SHTDENWK
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENWK     OF        XFDLIB
     JOINING     DEN2        AS        PREFIX.
*量販店備考ワーク
 FD  SHTBIKF
     LABEL       RECORD      IS        STANDARD.
     COPY        SHTDENWK    OF        XFDLIB
     JOINING     BIK         AS        PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*----<< 店舗マスタ >>-*
 FD  HTENMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HTENMS      OF        XFDLIB
     JOINING     TEN         AS        PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HMEIMS      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  RYO-ST              PIC  X(02)  VALUE  SPACE.
*****03  RYO-ST1             PIC  X(04)  VALUE  SPACE.
     03  DEN2-ST             PIC  X(02)  VALUE  SPACE.
*****03  DEN2-ST1            PIC  X(04)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
*****03  TOK-ST1             PIC  X(04)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
*****03  TEN-ST1             PIC  X(04)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
*****03  MEI-ST1             PIC  X(04)  VALUE  SPACE.
     03  BIK-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  RYO-ERR             PIC  N(10)  VALUE
                   NC"量販データＦ異常".
     03  DEN2-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"得意先マスタ異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  BIK-ERR             PIC  N(10)  VALUE
                   NC"量販店備考ワーク異常".
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 RYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTRYTF.
     DISPLAY     RYO-ERR     UPON      STAT.
     DISPLAY     RYO-ST      UPON      STAT.
*****DISPLAY     RYO-ST1     UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 DEN2-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
     DISPLAY     DEN2-ERR    UPON      STAT.
     DISPLAY     DEN2-ST     UPON      STAT.
*****DISPLAY     DEN2-ST1    UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 量販店備考ワーク >>--*
 BIK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTBIKF.
     DISPLAY     BIK-ERR     UPON      STAT.
     DISPLAY     BIK-ST      UPON      STAT.
     MOVE        255         TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      STAT.
     DISPLAY     TOK-ST      UPON      STAT.
*****DISPLAY     TOK-ST1     UPON      STAT.
     MOVE        255         TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR     UPON      STAT.
     DISPLAY     TEN-ST      UPON      STAT.
*****DISPLAY     TEN-ST1     UPON      STAT.
     MOVE        255         TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      STAT.
     DISPLAY     MEI-ST      UPON      STAT.
*****DISPLAY     MEI-ST1     UPON      STAT.
     MOVE        255         TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     OPEN        INPUT       SHTRYTF   HTOKMS   HTENMS   HMEIMS
                 I-O         SHTBIKF
                 OUTPUT      SHTDENWK.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
*量販データＲＥＡＤ
     READ     SHTRYTF   AT   END
              MOVE      9         TO   END-FLG
              GO        TO        MAIN-EXIT.
*項目転送
     PERFORM  VARYING   I    FROM   1  BY   1   UNTIL   I  >  50
         IF  (RYO-F162(I)  NOT =  ZERO)
              PERFORM   TEN-SEC
         END-IF
     END-PERFORM.
*項目転送（摘要）
     IF  RYO-F61   NOT  =    ZERO
         PERFORM   VARYING   I  FROM  1  BY   1   UNTIL   I  >  50
              IF  (RYO-F162(I)  NOT =  ZERO)
                   PERFORM   TEN-SEC2
              END-IF
         END-PERFORM
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                 項 目 転 送
****************************************************************
 TEN-SEC                     SECTION.
*伝票ＲＥＣ初期化
     MOVE        SPACE       TO   DEN2-REC.
     INITIALIZE  DEN2-REC.
*
     MOVE     RYO-F11        TO        DEN2-F01.
     MOVE     RYO-F011       TO        DEN2-F02.
     MOVE     RYO-F012       TO        DEN2-F03.
     MOVE     40             TO        DEN2-F051.
     MOVE     NC"売上伝票"   TO        DEN2-F052.
     MOVE     RYO-F03        TO        DEN2-F06.
     MOVE     RYO-F161(I)    TO        DEN2-F07.
     MOVE     RYO-F04        TO        DEN2-F08.
     MOVE     RYO-F05        TO        DEN2-F09.
     MOVE     RYO-F63        TO        DEN2-F44.
     MOVE     RYO-F071       TO        DEN2-F111.
     MOVE     RYO-F072       TO        DEN2-F112.
     MOVE     RYO-F072       TO        DEN2-F113.
     MOVE     RYO-F08        TO        DEN2-F12.
*
     MOVE     RYO-F09        TO        DEN2-F131.
     MOVE     RYO-F10        TO        DEN2-F132.
     MOVE     RYO-F60        TO        DEN2-F134.
     MOVE     RYO-F621       TO        DEN2-F1411.
     MOVE     RYO-F622       TO        DEN2-F1412.
     MOVE     RYO-F1211      TO        DEN2-F1421.
     MOVE     RYO-F1212      TO        DEN2-F1422.
     MOVE     RYO-F162(I)    TO        DEN2-F15.
*93/05/10. ﾂｲｶ S
     MOVE     RYO-F621       TO        MEI-F011.
     MOVE     RYO-F622       TO        MEI-F012.
     PERFORM  MEI-READ.
*\\ 94.12.01 START
*****IF       MEI-F93  =  1
*******MOVE     "1"            TO      DEN2-F16
*****END-IF.
     MOVE       "1"            TO      DEN2-F16.
*\\ 94.12.01 END
*
*93/05/10. ﾂｲｶ E
     MOVE     RYO-F131       TO        DEN2-F172.
     MOVE     RYO-F132       TO        DEN2-F173.
*
     COMPUTE  DEN2-F181 =    DEN2-F15  *    DEN2-F172.
     COMPUTE  DEN2-F182 =    DEN2-F15  *    DEN2-F173.
     MOVE     RYO-F15        TO        DEN2-F22.
     MOVE     RYO-F11        TO        TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F52        TO        DEN2-F24.
     MOVE     RYO-F121       TO        DEN2-F25.
     MOVE     RYO-F56        TO        DEN2-F26.
     MOVE     RYO-F57        TO        DEN2-F27.
*    93/02/18. ﾂｲｶ S
     MOVE     TOK-F89        TO        DEN2-F276.
     MOVE     TOK-F89        TO        DEN2-F27B.
*    93/02/18. ﾂｲｶ E
     MOVE     RYO-F58        TO        DEN2-F28.
     MOVE     RYO-F59        TO        DEN2-F29.
     MOVE     RYO-F11        TO        TEN-F52.
     MOVE     RYO-F161(I)    TO        TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F04        TO        DEN2-F30.
     MOVE     RYO-F99        TO        DEN2-F99.
*
     WRITE    DEN2-REC.
 TEN-EXIT.
     EXIT.
****************************************************************
*                 項 目 転 送
****************************************************************
 TEN-SEC2                    SECTION.
*量販店備考ワーク存在したら出力しない。
     MOVE        SPACE       TO   BIK-REC.
     INITIALIZE  BIK-REC.
     MOVE     RYO-F11        TO        BIK-F01.
     MOVE     RYO-F011       TO        BIK-F02.
     MOVE     80             TO        BIK-F03.
     READ     SHTBIKF
         INVALID
              CONTINUE
         NOT INVALID
              GO   TO        TEN-EXIT2
     END-READ.
*
*伝票ＲＥＣ初期化
     MOVE        SPACE       TO   BIK-REC.
     INITIALIZE  BIK-REC.
*
     MOVE     RYO-F11        TO        BIK-F01.
     MOVE     RYO-F011       TO        BIK-F02.
     MOVE     80             TO        BIK-F03.
     MOVE     40             TO        BIK-F051.
     MOVE     NC"売上伝票"   TO        BIK-F052.
     MOVE     RYO-F03        TO        BIK-F06.
     MOVE     RYO-F161(I)    TO        BIK-F07.
     MOVE     RYO-F04        TO        BIK-F08.
     MOVE     RYO-F05        TO        BIK-F09.
     MOVE     RYO-F63        TO        BIK-F44.
     MOVE     RYO-F071       TO        BIK-F111.
     MOVE     RYO-F072       TO        BIK-F112.
     MOVE     RYO-F072       TO        BIK-F113.
     MOVE     RYO-F08        TO        BIK-F12.
*
     MOVE     RYO-F09        TO        BIK-F131.
     MOVE     RYO-F10        TO        BIK-F132.
     MOVE     RYO-F60        TO        BIK-F134.
     MOVE     RYO-F61        TO        BIK-F1411.
     MOVE     RYO-F141       TO        BIK-F1421.
     MOVE     RYO-F142       TO        BIK-F1422.
     MOVE     RYO-F11        TO        TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F52        TO        BIK-F24.
     MOVE     RYO-F56        TO        BIK-F26.
     MOVE     RYO-F57        TO        BIK-F27.
     MOVE     RYO-F58        TO        BIK-F28.
     MOVE     RYO-F59        TO        BIK-F29.
     MOVE     RYO-F11        TO        TEN-F52.
     MOVE     RYO-F161(I)    TO        TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F04        TO        BIK-F30.
     MOVE     RYO-F99        TO        BIK-F99.
*
     WRITE    BIK-REC.
 TEN-EXIT2.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
     CLOSE    SHTRYTF   HTOKMS   SHTDENWK   HMEIMS  SHTBIKF.
 END-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ　 READ
****************************************************************
 TOK-READ                  SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F52
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ　 READ
****************************************************************
 TEN-READ                  SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F04
     END-READ.
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ　 READ
****************************************************************
 MEI-READ                  SECTION.
     READ     HMEIMS    INVALID
              MOVE      ZERO           TO   MEI-F93
     END-READ.
 MEI-READ-EXIT.
     EXIT.

```
